use std::time::Instant;

use std::time::Duration;

pub(crate) struct MeasureTask {
    pub init_duration: Duration,
    pub main_duration: Duration,
    pub total_duration: Duration,
}

impl MeasureTask {
    pub(crate) fn run<T>(label: Option<&str>, init: fn() -> T, main: fn(&mut T)) -> Self {
        let start = Instant::now();
        let mut state = init();
        let init_duration = start.elapsed();
        let start = Instant::now();
        for _ in 0..60 {
            main(&mut state);
        }
        let main_duration = start.elapsed();
        let total_duration = init_duration + main_duration;
        if let Some(l) = label {
            println!("\n===== {} =====", l);
            println!("Init duration:  {:?}", init_duration);
            println!("Main duration:  {:?}", main_duration);
            println!("Total duration: {:?}", total_duration);
            println!("=====================\n");
        }
        Self {
            init_duration,
            main_duration,
            total_duration,
        }
    }
}
