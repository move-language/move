// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use criterion::Criterion;
use std::time::{Duration, Instant};
pub struct TimeMeasurement;

struct TimeMeasurementFormatter;

impl TimeMeasurementFormatter {
    fn bytes_per_second(&self, bytes: f64, typical: f64, values: &mut [f64]) -> &'static str {
        let bytes_per_second = bytes * (1e9 / typical);
        let (denominator, unit) = if bytes_per_second < 1024.0 {
            (1.0, "  B/s")
        } else if bytes_per_second < 1024.0 * 1024.0 {
            (1024.0, "KiB/s")
        } else if bytes_per_second < 1024.0 * 1024.0 * 1024.0 {
            (1024.0 * 1024.0, "MiB/s")
        } else {
            (1024.0 * 1024.0 * 1024.0, "GiB/s")
        };

        for val in values {
            let bytes_per_second = bytes * (1e9 / *val);
            *val = bytes_per_second / denominator;
        }

        unit
    }

    fn elements_per_second(&self, elems: f64, typical: f64, values: &mut [f64]) -> &'static str {
        let elems_per_second = elems * (1e9 / typical);
        let (denominator, unit) = if elems_per_second < 1000.0 {
            (1.0, " elem/s")
        } else if elems_per_second < 1000.0 * 1000.0 {
            (1000.0, "Kelem/s")
        } else if elems_per_second < 1000.0 * 1000.0 * 1000.0 {
            (1000.0 * 1000.0, "Melem/s")
        } else {
            (1000.0 * 1000.0 * 1000.0, "Gelem/s")
        };

        for val in values {
            let elems_per_second = elems * (1e9 / *val);
            *val = elems_per_second / denominator;
        }

        unit
    }
}

impl criterion::measurement::ValueFormatter for TimeMeasurementFormatter {
    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        let (factor, unit) = if typical_value < 10f64.powi(0) {
            (10f64.powi(3), "ps")
        } else if typical_value < 10f64.powi(3) {
            (10f64.powi(0), "ns")
        } else if typical_value < 10f64.powi(6) {
            (10f64.powi(-3), "us")
        } else if typical_value < 10f64.powi(9) {
            (10f64.powi(-6), "ms")
        } else {
            (10f64.powi(-9), "s")
        };
        for val in values {
            *val *= factor;
        }
        unit
    }

    fn scale_throughputs(
        &self,
        typical_value: f64,
        throughput: &criterion::Throughput,
        values: &mut [f64],
    ) -> &'static str {
        match *throughput {
            criterion::Throughput::Bytes(bytes) => {
                self.bytes_per_second(bytes as f64, typical_value, values)
            }
            criterion::Throughput::Elements(elems) => {
                self.elements_per_second(elems as f64, typical_value, values)
            }
        }
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "ns"
    }
}

impl criterion::measurement::Measurement for TimeMeasurement {
    type Intermediate = Instant;
    type Value = Duration;

    fn start(&self) -> Self::Intermediate {
        Instant::now()
    }

    fn end(&self, i: Self::Intermediate) -> Self::Value {
        Instant::now().duration_since(i)
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        *v1 + *v2
    }

    fn zero(&self) -> Self::Value {
        Duration::from_secs(0)
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        value.as_nanos() as f64
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &TimeMeasurementFormatter
    }
}

pub fn cpu_time_measurement() -> Criterion<TimeMeasurement> {
    Criterion::default().with_measurement(TimeMeasurement)
}

pub fn wall_time_measurement() -> Criterion {
    Criterion::default()
}
