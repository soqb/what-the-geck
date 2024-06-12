macro_rules! define {
    ($($mod:ident => $variant:ident,)*) => {
        $(
            pub mod $mod;
            pub use $mod::$variant;
        )*

        #[derive(Debug, Serialize, Deserialize)]
        pub enum ParsedFields {
            $($variant($variant),)*
        }

        #[macro_export]
        macro_rules! field_map {
            ($view:ident) => {{
                $(
                    const $mod: &str = const_str::convert_ascii_case!(upper, stringify!($mod));
                )*
                $crate::records::read_data::map_fields! {$view;
                    $(
                        $mod => $variant,
                    )*
                }
            }};
        }

        pub use field_map;
        use serde::{Deserialize, Serialize};
    };
}

define! {
    tes4 => Tes4,
    txst => TextureSet,
    glob => Global,
    clas => Class,
    scpt => Script,
    qust => Quest,
    dial => Dialogue,
}
