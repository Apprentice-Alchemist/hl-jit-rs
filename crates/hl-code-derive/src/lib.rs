use virtue::prelude::*;

/// Derive macro generating an impl of the trait `hl_code::reader::Readable`
/// Use with care, in particular when Vecs are involved
#[proc_macro_derive(Readable)]
pub fn derive(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_inner(tokens).unwrap_or_else(|error| error.into_token_stream())
}
fn derive_inner(input: TokenStream) -> Result<TokenStream> {
    let parse = Parse::new(input)?;
    let (mut generator, _attributes, _body) = parse.into_generator();

    let target_name = generator.target_name();
    generator
        .impl_for("crate::reader::Readable")
        .generate_fn("r")
        .with_arg("r", "&mut crate::reader::Reader")
        .with_return_type("io::Result<Self>")
        .body(|b| {
            b.ident_str("let")
                .ident_str("ret")
                .punct('=');
            match _body {
                Body::Struct(s_body) => {
                    b.ident(target_name);
                    if let Some(ref fields) = s_body.fields {
                        emit_fields(fields, b)?;
                    }
                }
                Body::Enum(e) => {
                    b.ident_str("match")
                        .push_parsed("r.byte()?")?
                        .group(Delimiter::Brace, |b| {
                            for (idx, variant) in e.variants.iter().enumerate() {
                                b.lit_usize(idx).puncts("=>").ident_str("Self").puncts("::").ident(variant.name.clone());
                                if let Some(ref fields) = variant.fields {
                                    emit_fields(fields, b)?;
                                }
                                b.punct(',');
                            }
                            b.push_parsed(format!(
                                "kind => return Err(io::Error::new(io::ErrorKind::InvalidData, format!(\"invalid {} kind: {{kind}}\")))",
                                target_name.to_string()
                            ))?;
                            Ok(())
                        })?
                        .punct(';');
                }
            }
            b.punct(';');
            b.push_parsed("Ok(ret)")?;
            Ok(())
        })?;
    generator.export_to_file("hl-code", "Readable");
    generator.finish()
}

fn emit_fields(fields: &Fields, b: &mut StreamBuilder) -> Result<()> {
    match fields {
        Fields::Tuple(fields) => {
            b.group(Delimiter::Parenthesis, |b| {
                for field in fields {
                    if field.type_string() == "usize" {
                        b.push_parsed("r.udx()?")?;
                    } else {
                        b.push_parsed("r.r()?")?;
                    }
                    b.punct(',');
                }
                Ok(())
            })?;
        }
        Fields::Struct(fields) => {
            b.group(Delimiter::Brace, |b| {
                for (name, field) in fields {
                    b.ident(name.clone()).punct(':');
                    if field.type_string() == "usize" {
                        b.push_parsed("r.udx()?")?;
                    } else {
                        b.push_parsed("r.r()?")?;
                    }
                    b.punct(',');
                }
                Ok(())
            })?;
        }
    }
    Ok(())
}
