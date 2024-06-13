use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParamInfo {
    pub type_str: String,
    pub type_id: ParamType,
    pub optional: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandInfo {
    pub long_name: String,
    pub short_name: Option<String>,
    pub opcode: u32,
    pub help_text: String,
    pub needs_parent: bool,
    pub flags: u32,
    pub params: Vec<ParamInfo>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[repr(u32)]
pub enum ParamType {
    String = 0x00,
    Integer = 0x01,
    Float = 0x02,
    ObjectId = 0x03,
    ObjectRef = 0x04,
    ActorValue = 0x05,
    Actor = 0x06,
    SpellItem = 0x07,
    Axis = 0x08,
    Cell = 0x09,
    AnimationGroup = 0x0A,
    MagicItem = 0x0B,
    Sound = 0x0C,
    Topic = 0x0D,
    Quest = 0x0E,
    Race = 0x0F,
    Class = 0x10,
    Faction = 0x11,
    Sex = 0x12,
    Global = 0x13,
    Furniture = 0x14,
    TesObject = 0x15,
    VariableName = 0x16,
    QuestStage = 0x17,
    MapMarker = 0x18,
    ActorBase = 0x19,
    Container = 0x1A,
    WorldSpace = 0x1B,
    CrimeType = 0x1C,
    AiPackage = 0x1D,
    CombatStyle = 0x1E,
    MagicEffect = 0x1F,
    FormType = 0x20,
    WeatherId = 0x21,
    Npc = 0x22,
    Owner = 0x23,
    EffectShader = 0x24,
    FormList = 0x25,
    MenuIcon = 0x26,
    Perk = 0x27,
    Note = 0x28,
    MiscellaneousStat = 0x29,
    ImageSpaceModifier = 0x2A,
    ImageSpace = 0x2B,
    Double = 0x2C,
    ScriptVariable = 0x2D,
    #[serde(alias = "Unhandled2E")]
    VoiceType = 0x2E,
    EncounterZone = 0x2F,
    IdleForm = 0x30,
    Message = 0x31,
    InvObjOrFormList = 0x32,
    Alignment = 0x33,
    EquipType = 0x34,
    NonFormList = 0x35,
    SoundFile = 0x36,
    CriticalStage = 0x37,

    // added for dlc (1.1)
    #[serde(alias = "LeveledOrBaseChar")]
    LeveledOrBaseCharacter = 0x38,
    LeveledOrBaseCreature = 0x39,
    LeveledChar = 0x3A,
    LeveledCreature = 0x3B,
    LeveledItem = 0x3C,
    AnyForm = 0x3D,

    // new vegas
    Reputation = 0x3E,
    Casino = 0x3F,
    CasinoChip = 0x40,
    Challenge = 0x41,
    CaravanMoney = 0x42,
    CaravanCard = 0x43,
    CaravanDeck = 0x44,
    Region = 0x45,

    // nvse
    NvseArray = 0x100,
}

impl ParamType {
    pub const NVSE_STRING_VAR: ParamType = ParamType::Integer;
}
