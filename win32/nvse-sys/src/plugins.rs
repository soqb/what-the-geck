pub mod interfaces {
    pub enum Key {
        Serialization = 0,
        Console,
        Messaging,
        CommandTable,
        StringVar,
        ArrayVar,
        Script,
        Data,
        EventManager,
        Logging,
        Max,
    }
}

// struct NVSEInterface
// {
// 	UInt32	nvseVersion;
// 	UInt32	runtimeVersion;
// 	UInt32	editorVersion;
// 	UInt32	isEditor;
// 	bool	(* RegisterCommand)(CommandInfo * info);	// returns true for success, false for failure
// 	void	(* SetOpcodeBase)(UInt32 opcode);
// 	void *	(* QueryInterface)(UInt32 id);

// 	// call during your Query or Load functions to get a PluginHandle uniquely identifying your plugin
// 	// invalid if called at any other time, so call it once and save the result
// 	PluginHandle	(* GetPluginHandle)(void);

// 	// CommandReturnType enum defined in CommandTable.h
// 	// does the same as RegisterCommand but includes return type; *required* for commands returning arrays
// 	bool	(* RegisterTypedCommand)(CommandInfo * info, CommandReturnType retnType);
// 	// returns a full path the the game directory
// 	const char* (* GetRuntimeDirectory)();

// 	// Allows checking for nogore edition
// 	UInt32	isNogore;

// 	void		(*InitExpressionEvaluatorUtils)(ExpressionEvaluatorUtils *utils);
// };
