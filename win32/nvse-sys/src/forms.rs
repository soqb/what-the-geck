use std::{convert::Infallible, ffi::c_char, marker::PhantomData};

use crate::common::{TList, Unknown, Vtable, WinStr, WinString};

#[repr(C)]
pub struct BaseExtraData {
    vtable: &'static Vtable<()>,
    pub type_: u8,
    _00: [u8; 3],
    next: *const BaseExtraData,
}

#[repr(C)]
pub struct BaseExtraList {
    vtable: &'static Vtable<()>,
    pub data: *mut BaseExtraData,
    pub bits: [u8; 21],
    _00: [u8; 3],
}

type ExtraDataList = Infallible;
type NiNode = Infallible;
type AnimData = Infallible;
type ValidBip01Names = Infallible;
type NiPoint3 = Infallible;
type ObjectCell = Infallible;

#[repr(packed)]
pub(crate) struct ObjRefVtable {
    pub inherited: FormVtable,
    _02: [Unknown; 4],
    pub cast_shadows: unsafe extern "C" fn(&mut ObjRef) -> bool,
    _03: [Unknown; 12],
    pub remove_item: unsafe extern "C" fn(
        &mut ObjRef,
        *mut Form,
        *mut BaseExtraList,
        u32,
        u32,
        u32,
        &mut ObjRef,
        u32,
        u32,
        u32,
        u8,
    ) -> ObjRef,
    _04: [Unknown; 4],
    pub add_item: unsafe extern "C" fn(&mut ObjRef, *mut Form, *mut ExtraDataList, u32), // Needs confirmation
    _05: [Unknown; 3],
    pub get_is_child_size: unsafe extern "C" fn(&mut ObjRef, bool) -> bool, // 068 Actor: GetIsChildSize
    pub get_actor_unk0148: unsafe extern "C" fn(&mut ObjRef) -> u32, // result can be interchanged with baseForm, so TESForm* ?
    pub set_actor_unk0148: unsafe extern "C" fn(&mut ObjRef, u32),
    _06: [Unknown; 6],
    pub animate_ni_node: unsafe extern "C" fn(&mut ObjRef), // same in FOSE ! identical to Func0052 in OBSE which says (inits animation-related data, and more)
    pub generate_ni_node: unsafe extern "C" fn(&mut ObjRef, bool), // same in FOSE !
    pub set3_d: unsafe extern "C" fn(&mut ObjRef, *mut NiNode, bool), // same in FOSE !
    pub get_ni_node: unsafe extern "C" fn(&mut ObjRef) -> *mut NiNode, // same in FOSE !
    _07: [Unknown; 4],
    pub get_anim_data: unsafe extern "C" fn(&mut ObjRef) -> *mut AnimData, // 0079
    pub get_valid_bip01_names: unsafe extern "C" fn(&mut ObjRef) -> *mut ValidBip01Names, // 007A	Character only
    pub call_get_valid_bip01_names: unsafe extern "C" fn(&mut ObjRef) -> *mut ValidBip01Names,
    pub set_valid_bip01_names: unsafe extern "C" fn(&mut ObjRef, ValidBip01Names),
    pub get_pos: unsafe extern "C" fn(&ObjRef) -> *mut NiPoint3, // GetPos or GetDistance
    _08: [Unknown; 18],
}

#[repr(packed)]
pub(crate) struct BaseFormVtable {
    pub init: unsafe extern "C" fn(&mut BaseForm),
    pub free: unsafe extern "C" fn(&mut BaseForm),
    pub copy_from_base: unsafe extern "C" fn(&mut BaseForm, *const BaseForm),
    pub compare_with_base: unsafe extern "C" fn(&mut BaseForm, *const BaseForm) -> bool,
}

type ModInfo = Infallible;

#[repr(C)]
struct RtSwitchFn<A, B> {
    pub ptr: unsafe extern "C" fn(),
    _marker: PhantomData<fn() -> (A, B)>,
}

#[repr(C)]
struct EdSwitchFn<A, B> {
    pub ptr: unsafe extern "C" fn(),
    _marker: PhantomData<fn() -> (A, B)>,
}

#[repr(packed)]
pub(crate) struct FormVtable {
    pub inherited: BaseFormVtable,
    pub destroy: unsafe extern "C" fn(&mut Form, bool) -> *mut (),
    _00: [Unknown; 3],
    pub load_form: unsafe extern "C" fn(&mut Form, *const ModInfo),
    _01: [Unknown; 1],
    pub append_form: unsafe extern "C" fn(&mut Form, *mut ModInfo) -> bool,
    pub save_form: unsafe extern "C" fn(&mut Form),

    pub load_form2: unsafe extern "C" fn(&mut Form, *const ModInfo) -> bool,
    pub write_form_info: unsafe extern "C" fn(&mut Form, *mut ModInfo),
    _02: [Unknown; 1],
    pub sort: unsafe extern "C" fn(&mut Form, *mut Form) -> bool,
    pub create_form: unsafe extern "C" fn(&mut Form, *mut (), *mut ()) -> *mut Form,
    _03: [Unknown; 1],
    pub mark_as_modified: unsafe extern "C" fn(&mut Form, u32),
    pub mark_as_unmodified: unsafe extern "C" fn(&mut Form, u32),
    pub get_save_size: unsafe extern "C" fn(&mut Form, u32) -> u32,
    _04: [Unknown; 1],
    pub save_game: unsafe extern "C" fn(&mut Form, u32),
    pub load_game: unsafe extern "C" fn(&mut Form, *mut ()),
    pub load_game2: unsafe extern "C" fn(&mut Form, u32),
    _05: [Unknown; 3],
    pub revert: unsafe extern "C" fn(&mut Form, u32),
    _06: [Unknown; 5],
    pub init_item: unsafe extern "C" fn(&mut Form),
    pub get_type_id: unsafe extern "C" fn(&mut Form) -> u32,
    pub get_debug_name: unsafe extern "C" fn(&mut Form, *mut WinString),
    pub is_quest_item: unsafe extern "C" fn(&mut Form) -> bool,

    _07: [Unknown; 11],
    pub mark_for_deletion: unsafe extern "C" fn(&mut Form, bool),
    pub set_altered: unsafe extern "C" fn(&mut Form, bool),
    pub sw_set_quest_item: RtSwitchFn<unsafe extern "C" fn(&mut Form, bool), Unknown>,
    _08: [Unknown; 4],
    pub read_obndsub_record: unsafe extern "C" fn(&mut Form, *mut ModInfo),
    _09: [Unknown; 1],
    pub is_bound_object: unsafe extern "C" fn(&mut Form) -> bool,
    _0a: [Unknown; 1],
    pub sw_get_is_reference: RtSwitchFn<unsafe extern "C" fn(&Form) -> bool, Unknown>,
    _0b: [Unknown; 2],
    pub sw_get_edid_in_editor: RtSwitchFn<Unknown, unsafe extern "C" fn(&Form) -> *const c_char>,
    pub is_actor: unsafe extern "C" fn(&Form) -> bool,
    _0c: [Unknown; 1],
    pub copy_from: unsafe extern "C" fn(&mut Form, *mut Form),
    pub compare: unsafe extern "C" fn(&Form, *mut Form) -> bool,
    pub check_form_grup: unsafe extern "C" fn(&Form, *mut ()) -> bool,
    pub init_form_grup: unsafe extern "C" fn(&mut Form, *mut (), *mut ()),
    _0d: [Unknown; 4],
    pub set_ref_id: unsafe extern "C" fn(&mut Form, u32, bool),
    pub get_name2: unsafe extern "C" fn(&Form) -> *const c_char,
    pub get_name: unsafe extern "C" fn(&Form) -> *const c_char,

    pub set_editor_id_at_runtime: unsafe extern "C" fn(&mut Form, *const c_char) -> bool,
}

pub mod editor {
    use crate::common::WinString;

    #[repr(C)]
    pub struct FormData {
        pub editor_id: WinString,
        pub vc_master_form_id: u32,
        pub vc_revision: u32,
    }
    #[repr(C)]
    pub struct ObjRefData {
        _00: u32,
    }
}

mod cell {
    use crate::common::Vtable;

    #[repr(C)]
    pub struct ChildCell {
        vtable: &'static Vtable<()>,
    }
}

#[repr(C)]
#[non_exhaustive]
struct RenderState {
    _00: [u32; 2],
    pub water_level: f32,
    _01: [f32; 1],
    _02: [u32; 1],
    ni_node: *const NiNode,
}

#[repr(C)]
pub struct BaseForm {
    vtable: &'static Vtable<BaseFormVtable>,
}

#[repr(C)]
pub struct Form {
    vtable: &'static Vtable<FormVtable>,
    pub type_id: u8,
    _00: [u8; 3],
    pub flags: u32,
    pub ref_id: u32,
    #[cfg(feature = "editor")]
    pub editor_data: editor::FormData,
    pub mods: TList<ModInfo>,
}

#[repr(C)]
pub struct ObjRef {
    vtable: &'static Vtable<ObjRefVtable>,
    #[cfg(feature = "editor")]
    pub editor_data: editor::ObjRefData,
    pub child_cell: cell::ChildCell,
    _00: u32,
    pub base_form: *const Form,
    pub rot: [f32; 3],
    pub pos: [f32; 3],
    pub scale: f32,
    pub parent_cell: *const ObjectCell,
    pub extra_data_list: ExtraDataList,
    pub render_state: *const RenderState,
}

impl Form {
    pub fn get_name(&self) -> WinStr<'_> {
        // SAFETY: this method is assumed to be safe.
        unsafe { WinStr::from_ptr((self.vtable.table.get_name)(self)) }
    }
}

impl ObjRef {}
