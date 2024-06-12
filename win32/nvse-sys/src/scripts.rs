use std::convert::Infallible;

use crate::{
    common::{TList, Unknown, Vtable},
    forms::{Form, ObjRef},
};

type Quest = Infallible;
type RefList = Infallible;
type VarInfoList = Infallible;

use std::ffi::c_char;

use crate::{common::WinString, forms::FormVtable};

#[repr(C)]
struct RefVariable {
    pub name: WinString,
    pub form: Box<Form>,
    pub idx: u32,
}

#[repr(u8)]
pub enum VariableTy {
    Float = 0,
    Integer,
    String,
    Array,
    Ref,
    Invalid,
}

#[repr(C)]
pub struct VariableInfo {
    pub idx: u32,
    _00: [u32; 1],
    pub data: f64,
    pub type_: u8,
    _01: [u8; 7],
    pub name: WinString,
}

#[repr(C)]
pub struct ScriptInfo {
    pub unused_variable_cnt: u32,
    pub ref_cnt: u32,
    pub data_len: u32,
    pub var_cnt: u32,
    pub type_: u16,
    pub compiled: bool,
    _00: [u8; 1],
}

#[repr(C)]
pub struct Script {
    vtable: &'static Vtable<FormVtable>,
    #[cfg(not(feature = "runtime"))]
    _01: [u32; 1],
    pub info: ScriptInfo,
    pub text: *mut c_char,
    pub data: *mut u8,
    #[cfg(feature = "runtime")]
    _02: [u32; 1],
    #[cfg(feature = "runtime")]
    pub quest_delay_timer: f32,
    #[cfg(feature = "runtime")]
    pub seconds_passed: f32,
    #[cfg(feature = "runtime")]
    pub quest: *mut Quest,
    pub ref_list: TList<RefVariable>,
    pub var_list: TList<VariableInfo>,
    #[cfg(not(feature = "runtime"))]
    _03: [Unknown; 1],
    #[cfg(not(feature = "runtime"))]
    _04: [u8; 4],
}

// class Script : public TESForm
// {
// public:
// 	// members

// 	struct RefVariable
// 	{
// 		String name;   // 000 variable name/editorID (not used at run-time)
// 		TESForm *form; // 008
// 		UInt32 varIdx; // 00C

// 		void Resolve(ScriptEventList *eventList);

// 		Script *GetReferencedScript() const;
// 	};

// 	struct RefList : tList<RefVariable>
// 	{
// 		UInt32 GetIndex(Script::RefVariable *refVar);
// 	};

// 	enum VariableType : UInt8
// 	{
// 		eVarType_Float = 0, // ref is also zero
// 		eVarType_Integer,

// 		// NVSE, return values only
// 		eVarType_String,
// 		eVarType_Array,
// 		eVarType_Ref,

// 		eVarType_Invalid
// 	};

// 	struct VarInfoList : tList<VariableInfo>
// 	{
// 		VariableInfo *GetVariableByName(const char *name);
// 	};
// 	typedef Visitor<VarInfoList, VariableInfo> VarListVisitor;

// 	// 14
// 	struct ScriptInfo
// 	{
// 		UInt32 unusedVariableCount; // 00 (18)
// 		UInt32 numRefs;				// 04 (1C)
// 		UInt32 dataLength;			// 08 (20)
// 		UInt32 varCount;			// 0C (24)
// 		UInt16 type;				// 10 (28)
// 		bool compiled;				// 12 (2A)
// 		UInt8 unk13;				// 13 (2B)
// 	};

// 	enum
// 	{
// 		eType_Object = 0,
// 		eType_Quest = 1,
// 		eType_Magic = 0x100,
// 		eType_Unk = 0x10000,
// 	};
// #if !RUNTIME
// 	UInt32 unk028; //     /     / 028
// #endif
// 	ScriptInfo info; // 018 / 018 / 02C
// 	char *text;		 // 02C / 02C / 040
// 	UInt8 *data;	 // 030 / 030 / 044
// #if RUNTIME
// 	float unk34;				 // 034
// 	float questDelayTimeCounter; // 038      - init'd to fQuestDelayTime, decremented by frametime each frame
// 	float secondsPassed;		 // 03C      - only if you've modified fQuestDelayTime
// 	TESQuest *quest;			 // 040
// #endif
// 	RefList refList;	 // 044 / 034 / 048 - ref variables and immediates
// 	VarInfoList varList; // 04C / 03C / 050 - local variable list
// #if !RUNTIME
// 	void *unk050; //     /     / 050
// 	UInt8 unk054; //	   /     / 054
// 	UInt8 pad055[3];
// #endif

// 	RefVariable *GetRefFromRefList(UInt32 refIdx);
// 	VariableInfo *GetVariableInfo(UInt32 idx);

// 	UInt32 AddVariable(TESForm *form);
// 	void CleanupVariables(void);

// 	UInt32 Type() const { return info.type; }
// 	bool IsObjectScript() const { return info.type == eType_Object; }
// 	bool IsQuestScript() const { return info.type == eType_Quest; }
// 	bool IsMagicScript() const { return info.type == eType_Magic; }
// 	bool IsUnkScript() const { return info.type == eType_Unk; }

// 	VariableInfo *GetVariableByName(const char *varName);
// 	Script::VariableType GetVariableType(VariableInfo *var);

// 	bool IsUserDefinedFunction() const;

// 	static bool RunScriptLine(const char *text, TESObjectREFR *object = NULL);
// 	static bool RunScriptLine2(const char *text, TESObjectREFR *object = NULL, bool bSuppressOutput = true);

// 	// no changed flags (TESForm flags)
// 	MEMBER_FN_PREFIX(Script);
// #if RUNTIME
// 	// arg3 appears to be true for result scripts (runs script even if dataLength <= 4)
// 	DEFINE_MEMBER_FN(Execute, bool, kScript_ExecuteFnAddr, TESObjectREFR *thisObj, ScriptEventList *eventList, TESObjectREFR *containingObj, bool arg3);
// 	DEFINE_MEMBER_FN(Constructor, Script *, 0x005AA0F0);
// 	DEFINE_MEMBER_FN(SetText, void, 0x005ABE50, const char *text);
// 	DEFINE_MEMBER_FN(Run, bool, 0x005AC400, void *scriptContext, bool unkAlwaysOne, TESObjectREFR *object);
// 	DEFINE_MEMBER_FN(Destructor, void, 0x005AA1A0);
// #endif
// 	ScriptEventList *CreateEventList();
// 	UInt32 GetVarCount() const;
// 	UInt32 GetRefCount() const;

// 	tList<VariableInfo> *GetVars();
// 	tList<RefVariable> *GetRefList();

// 	void Delete();

// 	static game_unique_ptr<Script> MakeUnique();

// 	bool Compile(ScriptBuffer *buffer);
// };

impl ScriptRunner<'_> {
    const IF_STACK_DEPTH: usize = 10;
}

#[repr(C)]
pub struct Event {
    pub object: *const Form,
    pub event_mask: u32,
}

#[repr(C)]
pub struct ScriptEventList<'a> {
    pub script: &'a Script,
    _00: u32,
    pub event_list: &'a TList<Event>,
    pub vars: &'a TList<ScriptLocal>,
    _01: u64,
}

#[repr(C)]
pub struct ScriptLocal {
    pub id: u32,
    next_entry: *const ScriptLocal,
    pub data: f64,
}

#[repr(C)]
pub struct ScriptRunner<'a> {
    pub containing_obj: Option<&'a ObjRef>,
    pub calling_ref_base_form: &'a Form,
    pub script_event_list: &'a ScriptEventList<'a>,
    _00: [u32; 2],
    pub script: &'a Script,
    _01: [u32; 2],
    pub if_stack_depth: u32,
    pub if_stack: [u32; ScriptRunner::IF_STACK_DEPTH],
    pub invalid_references: bool,
    _02: [u8; 3],
}
