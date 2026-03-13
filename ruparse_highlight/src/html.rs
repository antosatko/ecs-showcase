use crate::{Span, Types};

pub struct MyStyle;

impl Style for MyStyle {
    fn color(&self, ty: Types) -> Color {
        match ty {
            Types::Comment => "#6A9955",         // Green
            Types::Ident => "#9CDCFE",           // Light Blue
            Types::Keyword => "#C586C0",         // Magenta/Purple
            Types::String => "#CE9178",          // Orange/Terracotta
            Types::Number => "#B5CEA8",          // Light Green/Yellow
            Types::Char => "#D7BA7D",            // Gold
            Types::Operator => "#D4D4D4",        // Light Gray
            Types::Type => "#4EC9B0",            // Teal
            Types::SpecialOperator => "#569CD6", // Distinct blue for control flow symbols
            Types::Label => "#DCDCAA",           // Soft yellow for labels
        }
    }
}

pub type Color = &'static str;

pub trait Style {
    fn color(&self, ty: Types) -> Color;

    fn render(&self, spans: &Vec<Span>, src: &str) -> String {
        let mut out =
            String::from("<pre style=\"background:#1E1E1E; color:#D4D4D4; padding:10px;\"><code>");

        // 1. Sort spans by offset to ensure linear rendering
        let mut sorted_spans = spans.clone();
        deduplicate_spans(&mut sorted_spans);
        sorted_spans.sort_by_key(|s| s.offset);

        let mut current_pos = 0;

        for span in sorted_spans {
            // Check for overlap or backward spans (safety)
            if span.offset < current_pos {
                continue;
            }

            // 2. Push text between the last span and this one (Plain Text)
            if span.offset > current_pos {
                let plain = &src[current_pos..span.offset];
                out.push_str(&escape_html(plain));
            }

            // 3. Push the highlighted span
            let end = span.offset + span.len;
            if let Some(text) = src.get(span.offset..end) {
                let color = self.color(unsafe { std::mem::transmute(span.ty) });
                out.push_str(&format!(
                    "<span style=\"color:{}\">{}</span>",
                    color,
                    escape_html(text)
                ));
            }

            current_pos = end;
        }

        // 4. Push remaining text after the last span
        if current_pos < src.len() {
            out.push_str(&escape_html(&src[current_pos..]));
        }

        out.push_str("</code></pre>");
        out
    }
}

fn deduplicate_spans(spans: &mut Vec<Span>) {
    // Sort by offset, then by length descending, then by Type priority
    spans.sort_by(|a, b| {
        a.offset
            .cmp(&b.offset)
            .then(b.len.cmp(&a.len))
            .then(b.ty.cmp(&a.ty))
    });
    spans.dedup_by(|a, b| a.offset == b.offset && a.len == b.len);
}
/// Simple helper to escape HTML entities
fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}
