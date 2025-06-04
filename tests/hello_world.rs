const BIN_NAME: &str = std::env!("CARGO_BIN_EXE_hl-jit");

#[test]
fn hello_world() {
    assert!(
        std::process::Command::new(BIN_NAME)
            .arg("tests/hello.hl")
            .status()
            .unwrap()
            .success()
    );
}
