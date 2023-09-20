#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! peek_match_tok {
    ($kind:pat) => {
        match self.input.peek() {
            Some(Ok($kind)) => Ok(true),
            Some(err @ Err(_)) => err,
            None => Ok(false),
        }?
    };
    ($kind:pat, $effect:block) => {{
        let matched = peek_match_tok!($kind);
        if matched {
            $effect?;
        }
        matched
    }};
    ($guard:expr) => {
        match self.input.peek() {
            Some(Ok(tok)) => Ok($guard(tok.kind)),
            Some(err @ Err(_)) => err,
            None => Ok(false),
        }?
    };
    ($guard:expr, $effect:block) => {{
        let matched = peek_match_tok!($guard);
        if matched {
            $effect?;
        }
        matched
    }};
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! block_tok {
    (|$kind:pat, $span:pat| $block:block) => {{
        let tok = self.input.next().expect("tok is some").expect("tok is ok");
        (|$kind, $span| -> std::result::Result<(), Error> {
            $block?;
            Ok(())
        })(tok.kind, tok.span)?;

        true
    }}
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! expect_tok {
    ($kind:pat, $span:pat, $expect:literal) => {
        let Token { kind: $kind, span: $span } = {
            let Some(res) = self.input.next() else {
                return Err(self.input.error(ErrorKind::UnexpectedEOF($expect.into())))
            }

            res?
        } else {
            return Err(self.input.error(ErrorKind::UnexpectedToken(tok.kind, $expect.into())));
        }
    };
    ($kind:pat, $expect:literal) => {
        expect_tok!($kind, _, $expect)
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! illegal_next_tok {
    ([$($tok:pat),+ $(,)*], $expect:literal) => {
        match $input.peek() {
            $(| Some(Ok(Token { kind: $tok, .. })))+ => {
                let illegal_tok = self.input.next().unwrap().unwrap();
                Err(self.input.error(ErrorKind::UnexpectedToken(
                    illegal_tok.kind,
                    $expect.to_string()
                )))
            },
            _ => Ok(()),
        }?;
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! match_tok {
    ($kind:pat, $span:pat) => {
        let Token { kind: $kind, span: $span } = {
            if !peek_match_tok!($kind) {
                return Ok(None);
            }

            self.input.next()?.unwrap()
        }
    }
    ($kind:pat) => {
        match_tok!($kind, _)
    }
    ($guard:expr) => {{
        if !peek_match_tok!($guard) {
            return Ok(None);
        }

        self.input.next()?.unwrap()
    }}
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! try_match_tok {
    (|$kind:pat| $block:block) => {
        try_match_tok!(|$kind, _| $block)
    }
    (|$kind:pat, $span:pat| $block:block) => {
        peek_match_tok!($kind, block_tok!(|$kind, $span| $block))
    }
    ($guard:expr, |$kind:pat| $block:block) => {
        try_match_tok!($guard, |$kind, _| $block)
    }
    ($guard:expr, |$kind:pat, $span:pat| $block:block) => {
        peek_match_tok!($guard, block_tok!(|$kind, $span| $block))
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! matches_tok {
    ($kind:pat) => {
        peek_match_tok!($kind, { self.input.next(); })
    };
    ($guard:expr) => {
        peek_match_tok!($guard, { self.input.next(); })
    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! match_one {
    ($input: expr; [ ($kind:pat, |$span: pat| $block:block) $(,)* ]) => {{
        if !matches_one!($input; [ ($kind, |$span| $block) ]) {
            return Ok(None);
        }

        true
    }};
    ($input: expr; [ ($guard:expr, |$span:pat, $kind:pat| $block:block) $(,)* ]) => {{
        if !matches_one!($input; [ ($guard, |$span, $kind| $block) ]) {
            return Ok(None);
        }

        true
    }};
    ($input: expr; [
        ($kind:pat, |$span:pat| $block:block),
        $(($kind_more:pat, |$span_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if !matches_one!($input; [
            ($kind, |$span| $block),
            $(($kind_more, |$span_more| $block_more),)+
        ]) {
            return Ok(None);
        }

        true
    }};
    ($input: expr; [
        ($guard:expr, |$span:pat, $kind:pat| $block:block),
        $(($guard_more:expr, |$span_more:pat, $kind_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if !matches_one!($input; [
            ($guard, |$span, $kind| $block),
            $(($guard_more, |$span_more, $kind_more| $block_more),)+
        ]) {
            return Ok(None);
        }

        true
    }};
}

#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! matches_one {
    ($input:expr; [ ($kind:pat, |$span: pat| $block:block) $(,)* ]) => {{
        matches_tok!($input; $kind, |$span| $block)
    }};
    ($input: expr; [ ($guard:expr, |$span:pat, $kind:pat| $block:block) $(,)* ]) => {{
        matches_tok!($input; $guard, |$span, $kind| $block)
    }};
    ($input:expr; [
        ($kind:pat, |$span:pat| $block:block),
        $(($kind_more:pat, |$span_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if matches_tok!($input; $kind, |$span| $block) {
            true
        } else {
            matches_one!($input; [ $(($kind_more, |$span_more| $block_more),)+ ])
        }
    }};
    ($input: expr; [
        ($guard:expr, |$span:pat, $kind:pat| $block:block),
        $(($guard_more:expr, |$span_more:pat, $kind_more:pat| $block_more:block)),+
        $(,)*
    ]) => {{
        if matches_tok!($input; $guard, |$span, $kind| $block) {
            true
        } else {
            matches_one!($input; [ $(($guard_more, |$span_more, $kind_more| $block_more),)+ ])
        }
    }};
}

pub(crate) use expect_tok;
pub(crate) use illegal_next_tok;
pub(crate) use match_one;
pub(crate) use match_tok;
pub(crate) use matches_one;
pub(crate) use matches_tok;
pub(crate) use try_match_tok;

