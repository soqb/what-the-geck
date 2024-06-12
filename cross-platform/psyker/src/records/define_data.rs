macro_rules! impl_from_fields {
    ($(#[$attr:meta])*
    $name:ident {
        $($key:expr => $field:ident: $field_ty:ty $(as $parse_ty:ty)?),*
        $(,)?
    }) => {
        #[derive(Debug, serde::Serialize, serde::Deserialize)]
        $(#[$attr])*
        pub struct $name {
            $(
                pub $field: $field_ty,
            )*
        }

        impl $crate::records::FromFields for $name {
            fn from_fields<'a, 'b, R>(view: &'b mut $crate::records::FieldsView<'a, R>) -> binrw::BinResult<Self>
            where
                'a: 'b,
                R: std::io::Read + std::io::Seek + 'a,
            {
                Ok(Self {
                    $(
                        $field: {
                            macro_rules! helper {
                                (adapter $first:tt) => {
                                    $crate::records::FieldAdapter::<'_, '_, R, $first>::adapt
                                };

                                (adapter $first:tt, $rest:tt) => {
                                    $crate::records::FieldAdapter::<'_, '_, R, $first>::adapt
                                };

                                (parse $first:tt) => {
                                    <$first as $crate::records::Parser<_>>::parse
                                };

                                (parse $first:tt, $rest:tt) => {
                                    <$first as $crate::records::Parser<_>>::parse
                                };
                            }

                            let parsed = helper!(adapter $($parse_ty,)? $field_ty)($key, &mut *view)
                                    .and_then(|input| {
                                        helper!(
                                            parse $($parse_ty,)? $field_ty
                                        )(input)
                                    })
                                    .or_else(|err| Err(binrw::Error::Custom {
                                        pos: view.reader.stream_position()?,
                                        err: Box::new(format!("{err:?}\n While parsing {} with parser {}", stringify!($key), stringify!($field_ty $(as $parse_ty)?))),
                                    })
                            )?;

                            $(
                                let parsed = <$parse_ty as $crate::records::ParsedInto::<_>>::parsed_into(parsed);
                            )?

                            parsed
                        },
                    )*
                })
            }
        }
    }
}
