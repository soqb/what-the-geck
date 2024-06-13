use std::{
    fs,
    io::{self, Cursor},
};

use binrw::Endian;
use fir::{
    ComponentInfo, Const, Diagnostic, EventInfo, FormInfo, Frontend, FunctionDefinition,
    FunctionInfo, FunctionReference, Ident4, Insert, Name, ParamInfo, RefTy, ResourcesMut,
    StopToken, TargetContext, Ty, TypeDefinition, TypeIdx, TypeInfo, VariableInfo,
};

use hashbrown::{HashMap, HashSet};
use nv_types::{CommandInfo, ParamType};
use psyker::{
    common::FormId,
    format::{EntryLabel, GroupLabel, ParseOptions, PluginEntry, Record},
};

#[derive(Default)]
pub struct Context {
    type_actor: Option<TypeIdx>,
    type_actor_base: Option<TypeIdx>,
    type_actor_value: Option<TypeIdx>,
    type_critical_stage: Option<TypeIdx>,
    type_animation_group: Option<TypeIdx>,
    type_axis: Option<TypeIdx>,
    type_sex: Option<TypeIdx>,
    type_alignment: Option<TypeIdx>,
    type_misc_stat: Option<TypeIdx>,
    type_owner: Option<TypeIdx>,
    type_lb_character: Option<TypeIdx>,
    type_lb_creature: Option<TypeIdx>,
    type_equip_type: Option<TypeIdx>,
    type_magic_item: Option<TypeIdx>,
    type_crime_type: Option<TypeIdx>,
    type_furniture: Option<TypeIdx>,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("io error")]
    Io(#[from] io::Error),
    #[error("deserialisation failed (cache)")]
    RonDeser(#[from] ron::error::SpannedError),
    #[error("deserialisation failed (binary)")]
    BinDeser(#[from] binrw::Error),
}

impl Diagnostic for Error {
    fn kind(&self) -> fir::DiagnosticKind {
        fir::DiagnosticKind::CompileFail
    }
}

fn name(name: &str) -> Name {
    Name {
        ident: name.to_owned(),
    }
}

impl Context {
    fn load_commands(&self) -> Result<Vec<CommandInfo>, Error> {
        let mut vec = Vec::new();

        for file in fs::read_dir("../win32/.cache")? {
            let content = fs::read_to_string(file?.path())?;
            let command: CommandInfo = ron::from_str(&content)?;

            vec.push(command);
        }

        Ok(vec)
    }

    fn load_records(&self, component: &mut impl Insert) -> Result<(), Error> {
        let start = std::time::Instant::now();
        let read = fs::read(
            "/home/seth/.local/share/Steam/steamapps/common/Fallout New Vegas/Data/FalloutNV.esm",
        )?;
        let mut read = Cursor::new(read);
        let mf = {
            let mut skip_groups = HashSet::new();
            skip_groups.insert(GroupLabel::Top(psyker::common::Ident4::new("LAND")));
            <psyker::format::MainFile as binrw::BinRead>::read_options(
                &mut read,
                Endian::Little,
                &ParseOptions {
                    ignore_entry: |label| match label {
                        EntryLabel::Record { label, .. } if label == "LAND" => true,
                        _ => false,
                    },
                },
            )?
        };
        let mid = std::time::Instant::now();
        let index = {
            fn build_index<'a>(
                index: &mut HashMap<FormId, &'a Record>,
                entries: impl IntoIterator<Item = &'a PluginEntry>,
            ) {
                for entry in entries.into_iter() {
                    match entry {
                        PluginEntry::Group { label: _, contents } => {
                            build_index(index, &contents.entries)
                        }
                        PluginEntry::Record { label: _, contents } => {
                            index.insert(contents.formid, contents);
                        }
                        PluginEntry::Ignored(_) => (),
                    }
                }
            }
            let mut index = HashMap::new();
            build_index(&mut index, &mf.top_level_entries);
            index
        };
        self.fold_forms(component, &index, &mf.top_level_entries)?;
        let end = std::time::Instant::now();
        eprintln!(
            "parsing took {n:3}s;\nfolding took {m:3}s",
            n = (mid - start).as_secs_f32(),
            m = (end - mid).as_secs_f32()
        );
        Ok(())
    }

    fn fold_forms<'a>(
        &self,
        component: &mut impl Insert,
        index: &HashMap<FormId, &'a Record>,
        entries: impl IntoIterator<Item = &'a PluginEntry>,
    ) -> Result<(), Error> {
        for entry in entries {
            match entry {
                PluginEntry::Group { label: _, contents } => {
                    self.fold_forms(component, index, &contents.entries)?;
                }
                PluginEntry::Record { label, contents } => {
                    if let Some(edid) = &contents.fields.common.editor_id {
                        let idx = component.insert_form(FormInfo {
                            name: fir::Name {
                                ident: edid.to_owned(),
                            },
                            kind: Ident4::from_str(label.as_str()),
                            script: None,
                        });

                        use psyker::records::*;
                        let script_fields = match &contents.fields.parsed {
                            Some(ParsedFields::Quest(quest)) => quest
                                .script
                                .and_then(|ref id| index.get(id))
                                .and_then(|x| match x.fields.parsed.as_ref()? {
                                    ParsedFields::Script(script) => Some(script),
                                    _ => None,
                                }),
                            Some(ParsedFields::Script(script)) => Some(script),
                            _ => None,
                        };
                        if let Some(script) = script_fields {
                            script
                                .variables
                                .iter()
                                .filter_map(|(name, var)| {
                                    Some(fir::VariableInfo {
                                        name: fir::Name {
                                            ident: name.0.clone(),
                                        },
                                        owning_form: Some(idx),
                                        ty: match var.type_ {
                                            scpt::Type::FloatOrRef => Ty::Float,
                                            scpt::Type::SomeInt => Ty::Integer,
                                            scpt::Type::Other => return None,
                                        },
                                    })
                                })
                                .for_each(|info| {
                                    component.insert_external_variable(info);
                                });
                        }
                    }
                }
                PluginEntry::Ignored(_) => (),
            }
        }

        Ok(())
    }

    fn map_param_type(&mut self, param: ParamType, component: &mut impl Insert) -> Ty {
        macro_rules! intern_type {
            ($opt_expr:expr, $name:ident = $type:expr $(,)?) => {
                Ty::Adt($opt_expr.unwrap_or_else(|| {
                    let idx = component.insert_type(TypeInfo {
                        name: name(stringify!($name)),
                        definition: $type,
                    });
                    $opt_expr = Some(idx);
                    idx
                }))
            };
        }

        macro_rules! ty_enum {
            ($($key:literal => $value:expr),* $(,)?) => {
                TypeDefinition::Enum {
                    map: [
                        $(($key.to_owned(), $value)),*
                    ].into_iter().collect(),
                    const_ty: Ty::Integer,
                }
            };
        }

        // fixme: consider strongly-typed `Ident4`s.
        macro_rules! ty_forms {
            (objects <- $($value:literal)|+) => {
                TypeDefinition::FormUnion {
                    is_object_ref: true,
                    fields: vec![$(Ident4::from_str($value)),*],
                }
            };
            (forms <- $($value:literal)|+) => {
                TypeDefinition::FormUnion {
                    is_object_ref: false,
                    fields: vec![$(Ident4::from_str($value)),*],
                }
            };
        }

        match param {
            ParamType::Integer => Ty::Integer,
            ParamType::Float => Ty::Float,
            ParamType::ObjectRef => Ty::Ref(RefTy::ANY_OBJECT),
            ParamType::String => Ty::String,
            ParamType::NvseArray => todo!(),
            ParamType::ActorValue => intern_type!(
                self.type_actor_value,
                ActorValue = ty_enum! {
                    "Aggression" => Const::Integer(0),
                    "Confidence" => Const::Integer(1),
                    "Energy" => Const::Integer(2),
                    "Responsibility" => Const::Integer(3),
                    "Mood" => Const::Integer(4),
                    "Strength" => Const::Integer(5),
                    "Perception" => Const::Integer(6),
                    "Endurance" => Const::Integer(7),
                    "Charisma" => Const::Integer(8),
                    "Intelligence" => Const::Integer(9),
                    "Agility" => Const::Integer(10),
                    "Luck" => Const::Integer(11),
                    "ActionPoints" => Const::Integer(12),
                    "CarryWeight" => Const::Integer(13),
                    "CritChance" => Const::Integer(14),
                    "HealRate" => Const::Integer(15),
                    "Health" => Const::Integer(16),
                    "MeleeDamage" => Const::Integer(17),
                    "DamageResist" => Const::Integer(18),
                    "PoisonResist" => Const::Integer(19),
                    "RadResist" => Const::Integer(20),
                    "SpeedMult" => Const::Integer(21),
                    "Fatigue" => Const::Integer(22),
                    "Karma" => Const::Integer(23),
                    "XP" => Const::Integer(24),
                    "PerceptionCondition" => Const::Integer(25),
                    "EnduranceCondition" => Const::Integer(26),
                    "LeftAttackCondition" => Const::Integer(27),
                    "RightAttackCondition" => Const::Integer(28),
                    "LeftMobilityCondition" => Const::Integer(29),
                    "RightMobilityCondition" => Const::Integer(30),
                    "BrainCondition" => Const::Integer(31),
                    "Barter" => Const::Integer(32),
                    "BigGuns" => Const::Integer(33),
                    "EnergyWeapons" => Const::Integer(34),
                    "Explosives" => Const::Integer(35),
                    "Lockpick" => Const::Integer(36),
                    "Medicine" => Const::Integer(37),
                    "MeleeWeapons" => Const::Integer(38),
                    "Repair" => Const::Integer(39),
                    "Science" => Const::Integer(40),
                    "Guns" => Const::Integer(41),
                    "SmallGuns" => Const::Integer(42), // maybe?
                    "Sneak" => Const::Integer(42),
                    "Speech" => Const::Integer(43),
                    "Survival" => Const::Integer(44),
                    "Unarmed" => Const::Integer(45),
                    "InventoryWeight" => Const::Integer(46),
                    "Paralysis" => Const::Integer(47),
                    "Invisibility" => Const::Integer(48),
                    "Chameleon" => Const::Integer(49),
                    "NightEye" => Const::Integer(50),
                    "Turbo" => Const::Integer(51),
                    "FireResist" => Const::Integer(52),
                    "WaterBreathing" => Const::Integer(53),
                    "RadiationRads" => Const::Integer(54),
                    "BloodyMess" => Const::Integer(55),
                    "UnarmedDamage" => Const::Integer(56),
                    "Assistance" => Const::Integer(57),
                    "ElectricResist" => Const::Integer(58),
                    "FrostResist" => Const::Integer(59),
                    "EnergyResist" => Const::Integer(60),
                    "EmpResist" => Const::Integer(61),
                    "Variable01" => Const::Integer(62),
                    "Variable02" => Const::Integer(63),
                    "Variable03" => Const::Integer(64),
                    "Variable04" => Const::Integer(65),
                    "Variable05" => Const::Integer(66),
                    "Variable06" => Const::Integer(67),
                    "Variable07" => Const::Integer(68),
                    "Variable08" => Const::Integer(69),
                    "Variable09" => Const::Integer(70),
                    "Variable10" => Const::Integer(71),
                    "IgnoreCrippledLimbs" => Const::Integer(72),
                    "Dehydration" => Const::Integer(73),
                    "Hunger" => Const::Integer(74),
                    "SleepDeprevation" => Const::Integer(75),
                    "DamageThreshold" => Const::Integer(76),
                },
            ),
            ParamType::MapMarker => Ty::form_ref("REFR"),
            ParamType::ActorBase => intern_type!(
                self.type_actor_base,
                ActorBase = ty_forms!(forms <- "ACHR" | "ACRE"),
            ),
            ParamType::CrimeType => intern_type!(
                self.type_crime_type,
                CrimeType = ty_enum! {
                    // fixme: not actually sure about the strings here.
                    "Theft" => Const::Integer(0),
                    "Pickpocketing" => Const::Integer(1),
                    "Trespassing" => Const::Integer(2),
                    "Assault" => Const::Integer(3),
                    "Murder" => Const::Integer(4),
                    // ..w .. what.?
                    "Lycanthropy" => Const::Integer(6),
                }
            ),
            ParamType::FormType => intern_type!(
                self.type_alignment,
                FormType = ty_enum! {
                    "NONE" => Const::Integer(0),
                    "TES4" => Const::Integer(1),
                    "GRUP" => Const::Integer(2),
                    "GMST" => Const::Integer(3),
                    "TXST" => Const::Integer(4),
                    "MICN" => Const::Integer(5),
                    "GLOB" => Const::Integer(6),
                    "CLAS" => Const::Integer(7),
                    "FACT" => Const::Integer(8),
                    "HDPT" => Const::Integer(9),
                    "HAIR" => Const::Integer(10),
                    "EYES" => Const::Integer(11),
                    "RACE" => Const::Integer(12),
                    "SOUN" => Const::Integer(13),
                    "ASPC" => Const::Integer(14),
                    "SKIL" => Const::Integer(15),
                    "MGEF" => Const::Integer(16),
                    "SCPT" => Const::Integer(17),
                    "LTEX" => Const::Integer(18),
                    "ENCH" => Const::Integer(19),
                    "SPEL" => Const::Integer(20),
                    "ACTI" => Const::Integer(21),
                    "TACT" => Const::Integer(22),
                    "TERM" => Const::Integer(23),
                    "ARMO" => Const::Integer(24),
                    "BOOK" => Const::Integer(25),
                    "CLOT" => Const::Integer(26),
                    "CONT" => Const::Integer(27),
                    "DOOR" => Const::Integer(28),
                    "INGR" => Const::Integer(29),
                    "LIGH" => Const::Integer(30),
                    "MISC" => Const::Integer(31),
                    "STAT" => Const::Integer(32),
                    "SCOL" => Const::Integer(33),
                    "MSTT" => Const::Integer(34),
                    "PWAT" => Const::Integer(35),
                    "GRAS" => Const::Integer(36),
                    "TREE" => Const::Integer(37),
                    "FLOR" => Const::Integer(38),
                    "FURN" => Const::Integer(39),
                    "WEAP" => Const::Integer(40),
                    "AMMO" => Const::Integer(41),
                    "NPC_" => Const::Integer(42),
                    "CREA" => Const::Integer(43),
                    "LVLC" => Const::Integer(44),
                    "LVLN" => Const::Integer(45),
                    "KEYM" => Const::Integer(46),
                    "ALCH" => Const::Integer(47),
                    "IDLM" => Const::Integer(48),
                    "NOTE" => Const::Integer(49),
                    "COBJ" => Const::Integer(50),
                    "PROJ" => Const::Integer(51),
                    "LVLI" => Const::Integer(52),
                    "WTHR" => Const::Integer(53),
                    "CLMT" => Const::Integer(54),
                    "REGN" => Const::Integer(55),
                    "NAVI" => Const::Integer(56),
                    "CELL" => Const::Integer(57),
                    "REFR" => Const::Integer(58),
                    "ACHR" => Const::Integer(59),
                    "ACRE" => Const::Integer(60),
                    "PMIS" => Const::Integer(61),
                    "PGRE" => Const::Integer(62),
                    "PBEA" => Const::Integer(63),
                    "PFLA" => Const::Integer(64),
                    "WRLD" => Const::Integer(65),
                    "LAND" => Const::Integer(66),
                    "NAVM" => Const::Integer(67),
                    "TLOD" => Const::Integer(68),
                    "DIAL" => Const::Integer(69),
                    "INFO" => Const::Integer(70),
                    "QUST" => Const::Integer(71),
                    "IDLE" => Const::Integer(72),
                    "PACK" => Const::Integer(73),
                    "CSTY" => Const::Integer(74),
                    "LSCR" => Const::Integer(75),
                    "LVSP" => Const::Integer(76),
                    "ANIO" => Const::Integer(77),
                    "WATR" => Const::Integer(78),
                    "EFSH" => Const::Integer(79),
                    "TOFT" => Const::Integer(80),
                    "EXPL" => Const::Integer(81),
                    "DEBR" => Const::Integer(82),
                    "IMGS" => Const::Integer(83),
                    "IMAD" => Const::Integer(84),
                    "FLST" => Const::Integer(85),
                    "PERK" => Const::Integer(86),
                    "BPTD" => Const::Integer(87),
                    "ADDN" => Const::Integer(88),
                    "AVIF" => Const::Integer(89),
                    "RADS" => Const::Integer(90),
                    "CAMS" => Const::Integer(91),
                    "CPTH" => Const::Integer(92),
                    "VTYP" => Const::Integer(93),
                    "IPCT" => Const::Integer(94),
                    "IPDS" => Const::Integer(95),
                    "ARMA" => Const::Integer(96),
                    "ECZN" => Const::Integer(97),
                    "MESG" => Const::Integer(98),
                    "RGDL" => Const::Integer(99),
                    "DOBJ" => Const::Integer(100),
                    "LGTM" => Const::Integer(101),
                    "MUSC" => Const::Integer(102),
                    "IMOD" => Const::Integer(103),
                    "REPU" => Const::Integer(104),
                    "PCBE" => Const::Integer(105),
                    "RCPE" => Const::Integer(106),
                    "RCCT" => Const::Integer(107),
                    "CHIP" => Const::Integer(108),
                    "CSNO" => Const::Integer(109),
                    "LSCT" => Const::Integer(110),
                    "MSET" => Const::Integer(111),
                    "ALOC" => Const::Integer(112),
                    "CHAL" => Const::Integer(113),
                    "AMEF" => Const::Integer(114),
                    "CCRD" => Const::Integer(115),
                    "CMNY" => Const::Integer(116),
                    "CDCK" => Const::Integer(117),
                    "DEHY" => Const::Integer(118),
                    "HUNG" => Const::Integer(119),
                    "SLPD" => Const::Integer(120),
                },
            ),
            ParamType::Owner => {
                intern_type!(self.type_owner, Owner = ty_forms!(forms <- "NPC_" | "FACT"))
            }
            ParamType::MenuIcon => Ty::form_ref("MICN"),
            ParamType::MiscellaneousStat => intern_type!(
                self.type_misc_stat,
                MiscellaneousStat = ty_enum! {
                    "Quests Completed" => Const::Integer(0),
                    "Locations Discovered" => Const::Integer(1),
                    "People Killed" => Const::Integer(2),
                    "Creatures Killed" => Const::Integer(3),
                    "Locks Picked" => Const::Integer(4),
                    "Computers Hacked"  => Const::Integer(5),
                    "Stimpaks Taken" => Const::Integer(6),
                    "Rad-X Taken" => Const::Integer(7),
                    "RadAway Taken" => Const::Integer(8),
                    "Chems Taken" => Const::Integer(9),
                    "Times Addicted" => Const::Integer(10),
                    "Mines Disarmed" => Const::Integer(11),
                    "Speech Successes" => Const::Integer(12),
                    "Pockets Picked" => Const::Integer(13),
                    "Pants Exploded" => Const::Integer(14),
                    "Books Read" => Const::Integer(15),
                    "Health From Stimpaks" => Const::Integer(16),
                    "Weapons Created" => Const::Integer(17),
                    "Health From Food" => Const::Integer(18),
                    "Water Consumed" => Const::Integer(19),
                    "Sandman Kills" => Const::Integer(20),
                    "Paralyzing Punches" => Const::Integer(21),
                    "Robots Disabled" => Const::Integer(22),
                    "Times Slept" => Const::Integer(23),
                    "Corpses Eaten" => Const::Integer(24),
                    "Mysterious Stranger Visits" => Const::Integer(25),
                    "Doctor Bags Used" => Const::Integer(26),
                    "Challenges Completed" => Const::Integer(27),
                    "Miss Fortunate Occurrences" => Const::Integer(28),
                    "Disintegrations" => Const::Integer(29),
                    "Have Limbs Crippled" => Const::Integer(30),
                    "Speech Failures" => Const::Integer(31),
                    "Items Crafted" => Const::Integer(32),
                    "Weapon Modifications" => Const::Integer(33),
                    "Items Repaired" => Const::Integer(34),
                    "Total Things Killed" => Const::Integer(35),
                    "Dismembered Limbs" => Const::Integer(36),
                    "Caravan Games Won" => Const::Integer(37),
                    "Caravan Games Lost" => Const::Integer(38),
                    "Barter Amount Traded" => Const::Integer(39),
                    "Roulette Games Played" => Const::Integer(40),
                    "Blackjack Games Played" => Const::Integer(41),
                    "Slots Games Played" => Const::Integer(42),
                },
            ),
            ParamType::Double => todo!(),
            ParamType::ScriptVariable => todo!(),
            ParamType::VoiceType => Ty::form_ref("VTYP"),
            // NB: it's a tossup between `IDLE` and `IDLM`. no functions with this parameter are ever used.
            ParamType::IdleForm => Ty::form_ref("IDLE"),
            ParamType::InvObjOrFormList => Ty::Ref(RefTy::ANY_FORM), // fixme: consider restricting?
            ParamType::NonFormList => Ty::Ref(RefTy::ANY_FORM),      // fixme: ditto
            ParamType::Alignment => intern_type!(
                self.type_alignment,
                KarmicAlignment = ty_enum! {
                    "VeryGood" => Const::Integer(0),
                    "Good" => Const::Integer(1),
                    "Neutral" => Const::Integer(2),
                    "Evil" => Const::Integer(3),
                    "VeryEvil" => Const::Integer(4),
                },
            ),
            ParamType::CriticalStage => intern_type!(
                self.type_critical_stage,
                CriticalStage = ty_enum! {
                    "None" => Const::Integer(0),
                    "GooStart" => Const::Integer(1),
                    "GooEnd" => Const::Integer(2),
                    "DisintegrateStart" => Const::Integer(3),
                    "DisintegrateEnd" => Const::Integer(4),
                },
            ),
            ParamType::LeveledOrBaseCharacter => intern_type!(
                self.type_lb_character,
                LeveledOrBaseChar = ty_forms!(forms <- "LVLN" | "ACHR"),
            ),
            ParamType::LeveledOrBaseCreature => intern_type!(
                self.type_lb_creature,
                LeveledOrBaseCreature = ty_forms!(forms <- "LVLC" | "ACRE"),
            ),
            ParamType::EquipType => intern_type!(
                self.type_equip_type,
                EquipType = ty_enum! {
                    "BigGuns" => Const::Integer(0),
                    "EnergyWeapons" => Const::Integer(1),
                    "SmallGuns" => Const::Integer(2),
                    "MeleeWeapons" => Const::Integer(3),
                    "UnarmedWeapons" => Const::Integer(4),
                    "ThrowWeapons" => Const::Integer(5),
                    "Mine" => Const::Integer(6),
                    "BodyWear" => Const::Integer(7),
                    "HeadWear" => Const::Integer(8),
                    "HandWear" => Const::Integer(9),
                    "Chems" => Const::Integer(10),
                    "Stimpack" => Const::Integer(11),
                    "Food" => Const::Integer(12),
                    "Alcohol" => Const::Integer(13),
                },
            ),
            ParamType::SpellItem => Ty::form_ref("BOOK"),
            ParamType::MagicItem => intern_type!(
                self.type_magic_item,
                MagicItem = ty_forms!(forms <- "ALCH" | "ENCH" | "SPEL"),
            ),
            ParamType::AnimationGroup => intern_type!(
                self.type_animation_group,
                AnimationGroup = ty_enum! {
                    "Idle" => Const::Integer(0),
                    "DynamicIdle" => Const::Integer(1),
                    "SpecialIdle" => Const::Integer(2),
                    "Forward" => Const::Integer(3),
                    "Backward" => Const::Integer(4),
                    "Left" => Const::Integer(5),
                    "Right" => Const::Integer(6),
                    "FastForward" => Const::Integer(7),
                    "FastBackward" => Const::Integer(8),
                    "FastLeft" => Const::Integer(9),
                    "FastRight" => Const::Integer(10),
                    "DodgeForward" => Const::Integer(11),
                    "DodgeBack" => Const::Integer(12),
                    "DodgeLeft" => Const::Integer(13),
                    "DodgeRight" => Const::Integer(14),
                    "TurnLeft" => Const::Integer(15),
                    "TurnRight" => Const::Integer(16),
                    "Aim" => Const::Integer(17),
                    "AimUp" => Const::Integer(18),
                    "AimDown" => Const::Integer(19),
                    "AimIS" => Const::Integer(20),
                    "AimISUp" => Const::Integer(21),
                    "AimISDown" => Const::Integer(22),
                    "Holster" => Const::Integer(23),
                    "Equip" => Const::Integer(24),
                    "Unequip" => Const::Integer(25),
                    "AttackLeft" => Const::Integer(26),
                    "AttackLeftUp" => Const::Integer(27),
                    "AttackLeftDown" => Const::Integer(28),
                    "AttackLeftIS" => Const::Integer(29),
                    "AttackLeftISUp" => Const::Integer(30),
                    "AttackLeftISDown" => Const::Integer(31),
                    "AttackRight" => Const::Integer(32),
                    "AttackRightUp" => Const::Integer(33),
                    "AttackRightDown" => Const::Integer(34),
                    "AttackRightIS" => Const::Integer(35),
                    "AttackRightISUp" => Const::Integer(36),
                    "AttackRightISDown" => Const::Integer(37),
                    "Attack3" => Const::Integer(38),
                    "Attack3Up" => Const::Integer(39),
                    "Attack3Down" => Const::Integer(40),
                    "Attack3IS" => Const::Integer(41),
                    "Attack3ISUp" => Const::Integer(42),
                    "Attack3ISDown" => Const::Integer(43),
                    "Attack4" => Const::Integer(44),
                    "Attack4Up" => Const::Integer(45),
                    "Attack4Down" => Const::Integer(46),
                    "Attack4IS" => Const::Integer(47),
                    "Attack4ISUp" => Const::Integer(48),
                    "Attack4ISDown" => Const::Integer(49),
                    "Attack5" => Const::Integer(50),
                    "Attack5Up" => Const::Integer(51),
                    "Attack5Down" => Const::Integer(52),
                    "Attack5IS" => Const::Integer(53),
                    "Attack5ISUp" => Const::Integer(54),
                    "Attack5ISDown" => Const::Integer(55),
                    "Attack6" => Const::Integer(56),
                    "Attack6Up" => Const::Integer(57),
                    "Attack6Down" => Const::Integer(58),
                    "Attack6IS" => Const::Integer(59),
                    "Attack6ISUp" => Const::Integer(60),
                    "Attack6ISDown" => Const::Integer(61),
                    "Attack7" => Const::Integer(62),
                    "Attack7Up" => Const::Integer(63),
                    "Attack7Down" => Const::Integer(64),
                    "Attack7IS" => Const::Integer(65),
                    "Attack7ISUp" => Const::Integer(66),
                    "Attack7ISDown" => Const::Integer(67),
                    "Attack8" => Const::Integer(68),
                    "Attack8Up" => Const::Integer(69),
                    "Attack8Down" => Const::Integer(70),
                    "Attack8IS" => Const::Integer(71),
                    "Attack8ISUp" => Const::Integer(72),
                    "Attack8ISDown" => Const::Integer(73),
                    "AttackLoop" => Const::Integer(74),
                    "AttackLoopUp" => Const::Integer(75),
                    "AttackLoopDown" => Const::Integer(76),
                    "AttackLoopIS" => Const::Integer(77),
                    "AttackLoopISUp" => Const::Integer(78),
                    "AttackLoopISDown" => Const::Integer(79),
                    "AttackSpin" => Const::Integer(80),
                    "AttackSpinUp" => Const::Integer(81),
                    "AttackSpinDown" => Const::Integer(82),
                    "AttackSpinIS" => Const::Integer(83),
                    "AttackSpinISUp" => Const::Integer(84),
                    "AttackSpinISDown" => Const::Integer(85),
                    "AttackSpin2" => Const::Integer(86),
                    "AttackSpin2Up" => Const::Integer(87),
                    "AttackSpin2Down" => Const::Integer(88),
                    "AttackSpin2IS" => Const::Integer(89),
                    "AttackSpin2ISUp" => Const::Integer(90),
                    "AttackSpin2ISDown" => Const::Integer(91),
                    "AttackPower" => Const::Integer(92),
                    "AttackForwardPower" => Const::Integer(93),
                    "AttackBackPower" => Const::Integer(94),
                    "AttackLeftPower" => Const::Integer(95),
                    "AttackRightPower" => Const::Integer(96),
                    "AttackCustom1Power" => Const::Integer(97),
                    "AttackCustom2Power" => Const::Integer(98),
                    "AttackCustom3Power" => Const::Integer(99),
                    "AttackCustom4Power" => Const::Integer(100),
                    "AttackCustom5Power" => Const::Integer(101),
                    "PlaceMine" => Const::Integer(102),
                    "PlaceMineUp" => Const::Integer(103),
                    "PlaceMineDown" => Const::Integer(104),
                    "PlaceMineIS" => Const::Integer(105),
                    "PlaceMineISUp" => Const::Integer(106),
                    "PlaceMineISDown" => Const::Integer(107),
                    "PlaceMine2" => Const::Integer(108),
                    "PlaceMine2Up" => Const::Integer(109),
                    "PlaceMine2Down" => Const::Integer(110),
                    "PlaceMine2IS" => Const::Integer(111),
                    "PlaceMine2ISUp" => Const::Integer(112),
                    "PlaceMine2ISDown" => Const::Integer(113),
                    "AttackThrow" => Const::Integer(114),
                    "AttackThrowUp" => Const::Integer(115),
                    "AttackThrowDown" => Const::Integer(116),
                    "AttackThrowIS" => Const::Integer(117),
                    "AttackThrowISUp" => Const::Integer(118),
                    "AttackThrowISDown" => Const::Integer(119),
                    "AttackThrow2" => Const::Integer(120),
                    "AttackThrow2Up" => Const::Integer(121),
                    "AttackThrow2Down" => Const::Integer(122),
                    "AttackThrow2IS" => Const::Integer(123),
                    "AttackThrow2ISUp" => Const::Integer(124),
                    "AttackThrow2ISDown" => Const::Integer(125),
                    "AttackThrow3" => Const::Integer(126),
                    "AttackThrow3Up" => Const::Integer(127),
                    "AttackThrow3Down" => Const::Integer(128),
                    "AttackThrow3IS" => Const::Integer(129),
                    "AttackThrow3ISUp" => Const::Integer(130),
                    "AttackThrow3ISDown" => Const::Integer(131),
                    "AttackThrow4" => Const::Integer(132),
                    "AttackThrow4Up" => Const::Integer(133),
                    "AttackThrow4Down" => Const::Integer(134),
                    "AttackThrow4IS" => Const::Integer(135),
                    "AttackThrow4ISUp" => Const::Integer(136),
                    "AttackThrow4ISDown" => Const::Integer(137),
                    "AttackThrow5" => Const::Integer(138),
                    "AttackThrow5Up" => Const::Integer(139),
                    "AttackThrow5Down" => Const::Integer(140),
                    "AttackThrow5IS" => Const::Integer(141),
                    "AttackThrow5ISUp" => Const::Integer(142),
                    "AttackThrow5ISDown" => Const::Integer(143),
                    "Attack9" => Const::Integer(144),
                    "Attack9Up" => Const::Integer(145),
                    "Attack9Down" => Const::Integer(146),
                    "Attack9IS" => Const::Integer(147),
                    "Attack9ISUp" => Const::Integer(148),
                    "Attack9ISDown" => Const::Integer(149),
                    "AttackThrow6" => Const::Integer(150),
                    "AttackThrow6Up" => Const::Integer(151),
                    "AttackThrow6Down" => Const::Integer(152),
                    "AttackThrow6IS" => Const::Integer(153),
                    "AttackThrow6ISUp" => Const::Integer(154),
                    "AttackThrow6ISDown" => Const::Integer(155),
                    "AttackThrow7" => Const::Integer(156),
                    "AttackThrow7Up" => Const::Integer(157),
                    "AttackThrow7Down" => Const::Integer(158),
                    "AttackThrow7IS" => Const::Integer(159),
                    "AttackThrow7ISUp" => Const::Integer(160),
                    "AttackThrow7ISDown" => Const::Integer(161),
                    "AttackThrow8" => Const::Integer(162),
                    "AttackThrow8Up" => Const::Integer(163),
                    "AttackThrow8Down" => Const::Integer(164),
                    "AttackThrow8IS" => Const::Integer(165),
                    "AttackThrow8ISUp" => Const::Integer(166),
                    "AttackThrow8ISDown" => Const::Integer(167),
                    "Counter" => Const::Integer(168),
                    "stomp" => Const::Integer(169),
                    "BlockIdle" => Const::Integer(170),
                    "BlockHit" => Const::Integer(171),
                    "Recoil" => Const::Integer(172),
                    "ReloadWStart" => Const::Integer(173),
                    "ReloadXStart" => Const::Integer(174),
                    "ReloadYStart" => Const::Integer(175),
                    "ReloadZStart" => Const::Integer(176),
                    "ReloadA" => Const::Integer(177),
                    "ReloadB" => Const::Integer(178),
                    "ReloadC" => Const::Integer(179),
                    "ReloadD" => Const::Integer(180),
                    "ReloadE" => Const::Integer(181),
                    "ReloadF" => Const::Integer(182),
                    "ReloadG" => Const::Integer(183),
                    "ReloadH" => Const::Integer(184),
                    "ReloadI" => Const::Integer(185),
                    "ReloadJ" => Const::Integer(186),
                    "ReloadK" => Const::Integer(187),
                    "ReloadL" => Const::Integer(188),
                    "ReloadM" => Const::Integer(189),
                    "ReloadN" => Const::Integer(190),
                    "ReloadO" => Const::Integer(191),
                    "ReloadP" => Const::Integer(192),
                    "ReloadQ" => Const::Integer(193),
                    "ReloadR" => Const::Integer(194),
                    "ReloadS" => Const::Integer(195),
                    "ReloadW" => Const::Integer(196),
                    "ReloadX" => Const::Integer(197),
                    "ReloadY" => Const::Integer(198),
                    "ReloadZ" => Const::Integer(199),
                    "JamA" => Const::Integer(200),
                    "JamB" => Const::Integer(201),
                    "JamC" => Const::Integer(202),
                    "JamD" => Const::Integer(203),
                    "JamE" => Const::Integer(204),
                    "JamF" => Const::Integer(205),
                    "JamG" => Const::Integer(206),
                    "JamH" => Const::Integer(207),
                    "JamI" => Const::Integer(208),
                    "JamJ" => Const::Integer(209),
                    "JamK" => Const::Integer(210),
                    "JamL" => Const::Integer(211),
                    "JamM" => Const::Integer(212),
                    "JamN" => Const::Integer(213),
                    "JamO" => Const::Integer(214),
                    "JamP" => Const::Integer(215),
                    "JamQ" => Const::Integer(216),
                    "JamR" => Const::Integer(217),
                    "JamS" => Const::Integer(218),
                    "JamW" => Const::Integer(219),
                    "JamX" => Const::Integer(220),
                    "JamY" => Const::Integer(221),
                    "JamZ" => Const::Integer(222),
                    "Stagger" => Const::Integer(223),
                    "Death" => Const::Integer(224),
                    "Talking" => Const::Integer(225),
                    "PipBoy" => Const::Integer(226),
                    "JumpStart" => Const::Integer(227),
                    "JumpLoop" => Const::Integer(228),
                    "JumpLand" => Const::Integer(229),
                    "HandGrip1" => Const::Integer(230),
                    "HandGrip2" => Const::Integer(231),
                    "HandGrip3" => Const::Integer(232),
                    "HandGrip4" => Const::Integer(233),
                    "HandGrip5" => Const::Integer(234),
                    "HandGrip6" => Const::Integer(235),
                    "JumpLoopForward" => Const::Integer(236),
                    "JumpLoopBackward" => Const::Integer(237),
                    "JumpLoopLeft" => Const::Integer(238),
                    "JumpLoopRight" => Const::Integer(239),
                    "PipBoyChild" => Const::Integer(240),
                    "JumpLandForward" => Const::Integer(241),
                    "JumpLandBackward" => Const::Integer(242),
                    "JumpLandLeft" => Const::Integer(243),
                    "JumpLandRight" => Const::Integer(244),
                },
            ),
            ParamType::AnyForm => Ty::Ref(RefTy::ANY_OBJECT),
            ParamType::ObjectId => Ty::Ref(RefTy::ANY_FORM),
            ParamType::Actor => intern_type!(
                self.type_actor,
                Actor = ty_forms!(objects <- "ACHR" | "ACRE"),
            ),
            ParamType::Cell => Ty::form_ref("CELL"), // todo: verify how accurate this is.
            ParamType::Container => Ty::form_ref("CONT"),
            ParamType::TesObject => Ty::Ref(RefTy::ANY_FORM),
            ParamType::SoundFile => Ty::form_ref("MUSC"),
            ParamType::Sound => Ty::form_ref("SOUN"),
            ParamType::Topic => Ty::form_ref("DIAL"),
            ParamType::Quest => Ty::form_ref("QUST"),
            ParamType::Race => Ty::form_ref("RACE"),
            ParamType::Class => Ty::form_ref("CLAS"),
            ParamType::Faction => Ty::form_ref("FACT"),
            ParamType::Global => Ty::form_ref("GLOB"),
            ParamType::Furniture => intern_type!(
                self.type_furniture,
                Furniture = ty_forms!(forms <- "FURN" | "FLST"),
            ),
            ParamType::WorldSpace => Ty::form_ref("WRLD"),
            ParamType::AiPackage => Ty::form_ref("PACK"),
            ParamType::CombatStyle => Ty::form_ref("CSTY"),
            ParamType::MagicEffect => Ty::form_ref("MGEF"), // i'm not sure if this is used.
            ParamType::WeatherId => Ty::form_ref("WTHR"), // this is a good guess. i have no evidence to support this definition
            ParamType::Npc => Ty::form_ref("NPC_"),
            ParamType::EffectShader => Ty::form_ref("EFSH"),
            ParamType::FormList => Ty::form_ref("FLST"),
            ParamType::Perk => Ty::form_ref("PERK"),
            ParamType::Note => Ty::form_ref("NOTE"),
            ParamType::ImageSpaceModifier => Ty::form_ref("IMAD"),
            ParamType::ImageSpace => Ty::form_ref("IMGS"),
            ParamType::EncounterZone => Ty::form_ref("ECZN"),
            ParamType::Message => Ty::form_ref("MESG"),
            ParamType::LeveledItem => Ty::form_ref("LVLI"),
            ParamType::LeveledChar => Ty::form_ref("LVLN"),
            ParamType::LeveledCreature => Ty::form_ref("LVNC"),
            ParamType::Reputation => Ty::form_ref("REPU"),
            ParamType::Casino => Ty::form_ref("CSNO"),
            ParamType::CasinoChip => Ty::form_ref("CHIP"),
            ParamType::Challenge => Ty::form_ref("CHIP"),
            ParamType::CaravanMoney => Ty::form_ref("CMNY"),
            ParamType::CaravanCard => Ty::form_ref("CCRD"),
            ParamType::CaravanDeck => Ty::form_ref("CDCK"),
            ParamType::Region => Ty::form_ref("REGN"),
            ParamType::QuestStage => Ty::Integer,
            ParamType::Axis => intern_type!(
                self.type_axis,
                Axis = ty_enum! {
                    "X" => Const::Integer(b'X' as i64),
                    "Y" => Const::Integer(b'Y' as i64),
                    "Z" => Const::Integer(b'Z' as i64),
                },
            ),
            ParamType::Sex => intern_type!(
                self.type_sex,
                Sex = ty_enum! {
                    // fixme: investigate adding an enby option.
                    // ..
                    // misogyny.
                    "Male" => Const::Integer(0),
                    "Female" => Const::Integer(1),
                },
            ),
            // fixme: i am so tired.
            ParamType::VariableName => Ty::Unit,
        }
    }

    fn insert_events(&self, component: &mut impl Insert) {
        fn zero() -> Vec<ParamInfo> {
            vec![]
        }

        fn one(ty: Ty) -> Vec<ParamInfo> {
            vec![ParamInfo {
                ty,
                optional: false,
                name: None,
            }]
        }

        fn zero_to_one(ty: Ty) -> Vec<ParamInfo> {
            vec![ParamInfo {
                ty,
                optional: true,
                name: None,
            }]
        }

        macro_rules! map {
            ($($key:literal => $value:expr),* $(,)?) => {
                [$(($key, $value)),*]
            };
        }

        // todo: it really looks like clone-on-write would help out all these vecs.
        let map = map! {
            "GameMode" => zero(),
            "MenuMode" => zero_to_one(Ty::Integer),
            "OnActivate" => zero(),
            "OnActorEquip" => one(Ty::Ref(RefTy::ANY_FORM)),
            "OnActorUnequip" => one(Ty::Ref(RefTy::ANY_FORM)),
            "OnAdd" => zero_to_one(Ty::object_ref("CONT")),
            "OnClose" => zero(),
            "OnCombatEnd" => zero_to_one(Ty::object_ref("NPC_")),
            "OnDeath" => zero_to_one(Ty::object_ref("NPC_")),
            "OnDestructionStageChange" => zero(),
            "OnDrop" => zero_to_one(Ty::object_ref("CONT")),
            "OnEquip" => zero_to_one(Ty::object_ref("NPC_")),
            "OnFire" => zero(),
            "OnGrab" => zero(),
            "OnHit" => zero_to_one(Ty::object_ref("NPC_")),
            "OnHitWith" => zero_to_one(Ty::Ref(RefTy::ANY_FORM)),
            "OnLoad" => zero(),
            "OnMagicEffectHit" => zero_to_one(Ty::form_ref("MGEF")),
            "OnMurder" => zero_to_one(Ty::object_ref("NPC_")),
            "OnNPCActivate" => zero(),
            "OnOpen" => zero(),
            "OnPackageChange" => one(Ty::form_ref("PACK")),
            "OnPackageDone" => one(Ty::form_ref("PACK")),
            "OnPackageEnd" => one(Ty::form_ref("PACK")),
            "OnPackageStart" => one(Ty::form_ref("PACK")),
            "OnRelease" => zero(),
            "OnReset" => zero(),
            "OnSell" => zero_to_one(Ty::object_ref("NPC_")),
            "OnStartCombat" => zero_to_one(Ty::object_ref("NPC_")),
            "OnTrigger" => zero_to_one(Ty::Ref(RefTy::ANY_OBJECT)),
            "OnTriggerEnter" => zero_to_one(Ty::Ref(RefTy::ANY_OBJECT)),
            "OnTriggerLeave" => zero_to_one(Ty::Ref(RefTy::ANY_OBJECT)),
            "OnUnequip" => zero_to_one(Ty::object_ref("NPC_")),
            "SayToDone" => zero_to_one(Ty::form_ref("NPC_")),
            "ScriptEffectFinish" => zero(),
            "ScriptEffectStart" => zero(),
            "ScriptEffectUpdate" => zero(),
        };
        for (event_name, parameters) in map {
            component.insert_event(EventInfo {
                name: name(event_name),
                parameters,
            });
        }
    }
}

impl<R: ResourcesMut> TargetContext<R> for Context {
    fn install<F: Frontend>(
        mut self,
        frontend: &mut F,
        resources: &mut R,
    ) -> Result<(), StopToken> {
        let mut component = resources.new_component_cx(ComponentInfo {
            identifier: "nvse".to_owned(),
            ..Default::default()
        });

        let commands = self
            .load_commands()
            .or_else(|err| frontend.report(err).map(|_| Vec::new()))?;
        for command in commands {
            let func = FunctionInfo {
                name: Name {
                    ident: command.long_name,
                },
                reference: FunctionReference::Defined(FunctionDefinition {
                    // fixme: verify & specialise where applicable.
                    self_param: command.needs_parent.then(|| Ty::Ref(RefTy::ANY_OBJECT)),
                    params: command
                        .params
                        .into_iter()
                        .map(|param| ParamInfo {
                            ty: self.map_param_type(param.type_id, &mut component),
                            optional: param.optional,
                            name: None,
                        })
                        .collect(),
                    return_ty: Ty::Float,
                }),
            };
            let func_idx = component.insert_function(func);
            if let Some(alias) = command.short_name {
                component.insert_function(FunctionInfo {
                    name: Name { ident: alias },
                    reference: FunctionReference::Alias {
                        aliased_function: func_idx,
                    },
                });
            }
        }

        self.insert_events(&mut component);
        self.load_records(&mut component)
            .or_else(|err| frontend.report(err))?;

        component.insert_external_variable(VariableInfo {
            name: name("player"),
            owning_form: None,
            ty: Ty::object_ref("ACHR"),
        });
        component.insert_external_variable(VariableInfo {
            name: name("playerRef"),
            owning_form: None,
            ty: Ty::object_ref("ACHR"),
        });

        Ok(())
    }
}
