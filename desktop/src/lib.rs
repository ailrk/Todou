use tauri_plugin_shell::ShellExt;
use tauri_plugin_shell::process::CommandEvent;

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_shell::init())
        .setup(|app| {
            if cfg!(debug_assertions) {
                app.handle().plugin(
                    tauri_plugin_log::Builder::default()
                        .level(log::LevelFilter::Info)
                        .build(),
                )?;
            }

            // Spawn Haskell backend as a sidecar
            // The name "todou" must match the prefix in desktop/binaries/
            let sidecar_command = app.shell().sidecar("todou").map_err(|e| {
                eprintln!("Failed to create sidecar command: {}", e);
                e
            })?;

            // Pass parameters here
            let (mut rx, _child) = sidecar_command
                .args(["--port=5555", "--storage=dir:_cache"])
                .spawn()
                .map_err(|e| {
                    eprintln!("Failed to spawn Haskell backend: {}", e);
                    e
                })?;

            tauri::async_runtime::spawn(async move {
                while let Some(event) = rx.recv().await {
                    match event {
                        CommandEvent::Stdout(line) => {
                            print!("HASKELL STDOUT: {}", String::from_utf8_lossy(&line));
                        }
                        CommandEvent::Stderr(line) => {
                            eprint!("HASKELL STDERR: {}", String::from_utf8_lossy(&line));
                        }
                        _ => {}
                    }
                }
            });

            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
