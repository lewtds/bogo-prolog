key_effect(f, add_tone(tone_huyen)).
key_effect(s, add_tone(tone_sac)).
key_effect(r, add_tone(tone_hoi)).
key_effect(x, add_tone(tone_nga)).
key_effect(j, add_tone(tone_nang)).

% TODO: Maybe having separate effects for â, ê, ô; same with ơ and ư
key_effect(a, add_vowel_mod(mod_hat)).
key_effect(e, add_vowel_mod(mod_hat)).
key_effect(o, add_vowel_mod(mod_hat)).
key_effect(w, add_vowel_mod(mod_horn)).
key_effect(w, add_vowel_mod(mod_breve)).

key_effect(d, add_consonant_mod(mod_dash)).

% Usage example:
%
% ?- process_key_sequence([b, a, n, s, w], [] , Output).
%    Output = [b, ắ, n].
%
process_key_sequence([Key|Rest], CurrentString, Output) :-
    once(process_key(CurrentString, Key, Output1)),
    process_key_sequence(Rest, Output1, Output).

process_key_sequence([], CurrentString, CurrentString).

% Try applying the effect of the key, assuming that there is one. The resulting string must be different.
% Performance optimization idea: give out both an output string and a syllable term and reuse the term for the next cycle.
process_key(CurrentString, Key, OutString) :-
    % break the syllable down into initial consonant, vowel nucleus and final consonant
    once(phrase(syllable(I, V, F), CurrentString)),

    apply_key_effect(s(I, V, F), Key, s(I2, V2, F2)),
    shortcut_rule(I2, V2, F2, I3, V3, F3),

    atomic_list_concat([I3, V3, F3], OutputAtom),
    atom_chars(OutputAtom, OutString),
    OutString \= CurrentString.

shortcut_rule(I, ưo, F, I, ươ, F).
shortcut_rule(I, V, F, I, V, F).

% otherwise, append the key to the end of the current string
process_key(CurrentString, Key, OutString) :-
    append(CurrentString, [Key], OutString).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_tone(Tone)),
    syllable_tone(InSyllable, Tone, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_vowel_mod(Mod)),
    syllable_vowel_mod(InSyllable, Key, Mod, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_consonant_mod(Mod)),
    syllable_consonant_mod(InSyllable, Mod, OutSyllable).

apply_key_effect(s(I, V, F), Key, s(I, V, F2)) :- atom_concat(F, Key, F2).
apply_key_effect(s(I, V, ''), Key, s(I, V2, '')) :- atom_concat(V, Key, V2).
apply_key_effect(s(I, '', ''), Key, s(I2, '', '')) :- atom_concat(I, Key, I2).

syllable_vowel_mod(s(I, V, F), Key, Mod, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(V, V_raw, _, Tone, _),
    vowel_nucleus_mod_tone(V_out, V_raw, Mod, Tone, Key).

syllable_tone(s(I, V, F), Tone, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(V, V_raw, Mod, _, _),
    vowel_nucleus_mod_tone(V_out, V_raw, Mod, Tone, _).

% there is the only case of d -> đ here so it's hard-coded
syllable_consonant_mod(s(d, V, F), _, s(đ, V, F)).

%%%%%%%%%%%%%
% Parser section
%%%%%%%%%%%%%

syllable(I, V, F) --> consonant_initial(I), vowel(V), consonant_final(F).

vowel(V) --> vowel_chars(Vs), { atom_chars(V, Vs) }.

vowel_chars([X|Rest]) --> vowel_char(X), vowel_chars(Rest).
vowel_chars([]) --> [].

vowel_char(C) --> [C], {
    atom_chars(àáảãạaằắẳẵặăầấẩẫậâèéẻẽẹeềếểễệêìíỉĩịiòóỏõọoồốổỗộôờớởỡợơùúủũụuừứửữựưỳýỷỹỵy, AtomList),
    member(C, AtomList)
}.

consonant_initial('') --> [].
consonant_initial(m) --> [m].
consonant_initial(n) --> [n].
consonant_initial(nh) --> [n, h].
consonant_initial(ng) --> [n, g].
consonant_initial(ngh) --> [n, g, h].
consonant_initial(p) --> [p].
consonant_initial(t) --> [t].
consonant_initial(tr) --> [t, r].
consonant_initial(ch) --> [c, h].
consonant_initial(c) --> [c].
consonant_initial(k) --> [k].
consonant_initial(qu) --> [q, u].
consonant_initial(ph) --> [p, h].
consonant_initial(th) --> [t, h].
consonant_initial(kh) --> [k, h].
consonant_initial(b) --> [b].
consonant_initial(đ) --> [đ].
consonant_initial(s) --> [s].
consonant_initial(x) --> [x].
consonant_initial(h) --> [h].
consonant_initial(d) --> [d].
consonant_initial(gi) --> [g, i].
consonant_initial(g) --> [g].
consonant_initial(gh) --> [g, h].
consonant_initial(v) --> [v].
consonant_initial(l) --> [l].
consonant_initial(r) --> [r].

consonant_final('') --> [].
consonant_final(n) --> [n].
consonant_final(nh) --> [n, h].
consonant_final(ng) --> [n, g].
consonant_final(c) --> [c].
consonant_final(ch) --> [c, h].
consonant_final(p) --> [p].
consonant_final(t) --> [t].
consonant_final(n) --> [n].
consonant_final(m) --> [m].


% vowel nucleus transformation database schema:
%   resulting vowel, base characters, vowel modification, tone, vowel modification affinity (eg. a + a -> â)
%
vowel_nucleus_mod_tone(a, a, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(à, a, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(á, a, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ả, a, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ã, a, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ạ, a, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(â, a, mod_hat, tone_ngang, 'a').
vowel_nucleus_mod_tone(ầ, a, mod_hat, tone_huyen, 'a').
vowel_nucleus_mod_tone(ấ, a, mod_hat, tone_sac, 'a').
vowel_nucleus_mod_tone(ẩ, a, mod_hat, tone_hoi, 'a').
vowel_nucleus_mod_tone(ẫ, a, mod_hat, tone_nga, 'a').
vowel_nucleus_mod_tone(ậ, a, mod_hat, tone_nang, 'a').

vowel_nucleus_mod_tone(ă, a, mod_breve, tone_ngang, 'w').
vowel_nucleus_mod_tone(ằ, a, mod_breve, tone_huyen, 'w').
vowel_nucleus_mod_tone(ắ, a, mod_breve, tone_sac, 'w').
vowel_nucleus_mod_tone(ẳ, a, mod_breve, tone_hoi, 'w').
vowel_nucleus_mod_tone(ẵ, a, mod_breve, tone_nga, 'w').
vowel_nucleus_mod_tone(ặ, a, mod_breve, tone_nang, 'w').

vowel_nucleus_mod_tone(e, e, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(è, e, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(é, e, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ẻ, e, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ẽ, e, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ẹ, e, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ê, e, mod_hat, tone_ngang, 'e').
vowel_nucleus_mod_tone(ề, e, mod_hat, tone_huyen, 'e').
vowel_nucleus_mod_tone(ế, e, mod_hat, tone_sac, 'e').
vowel_nucleus_mod_tone(ể, e, mod_hat, tone_hoi, 'e').
vowel_nucleus_mod_tone(ễ, e, mod_hat, tone_nga, 'e').
vowel_nucleus_mod_tone(ệ, e, mod_hat, tone_nang, 'e').

vowel_nucleus_mod_tone(i, i, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ì, i, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(í, i, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ỉ, i, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ĩ, i, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ị, i, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(o, o, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ò, o, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(ó, o, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ỏ, o, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(õ, o, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ọ, o, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ô, o, mod_hat, tone_ngang, 'o').
vowel_nucleus_mod_tone(ồ, o, mod_hat, tone_huyen, 'o').
vowel_nucleus_mod_tone(ố, o, mod_hat, tone_sac, 'o').
vowel_nucleus_mod_tone(ổ, o, mod_hat, tone_hoi, 'o').
vowel_nucleus_mod_tone(ỗ, o, mod_hat, tone_nga, 'o').
vowel_nucleus_mod_tone(ộ, o, mod_hat, tone_nang, 'o').

vowel_nucleus_mod_tone(ơ, o, mod_horn, tone_ngang, 'w').
vowel_nucleus_mod_tone(ờ, o, mod_horn, tone_huyen, 'w').
vowel_nucleus_mod_tone(ớ, o, mod_horn, tone_sac, 'w').
vowel_nucleus_mod_tone(ở, o, mod_horn, tone_hoi, 'w').
vowel_nucleus_mod_tone(ỡ, o, mod_horn, tone_nga, 'w').
vowel_nucleus_mod_tone(ợ, o, mod_horn, tone_nang, 'w').

vowel_nucleus_mod_tone(u, u, mod_none, tone_ngang, 'u').
vowel_nucleus_mod_tone(ù, u, mod_none, tone_huyen, 'u').
vowel_nucleus_mod_tone(ú, u, mod_none, tone_sac, 'u').
vowel_nucleus_mod_tone(ủ, u, mod_none, tone_hoi, 'u').
vowel_nucleus_mod_tone(ũ, u, mod_none, tone_nga, 'u').
vowel_nucleus_mod_tone(ụ, u, mod_none, tone_nang, 'u').

vowel_nucleus_mod_tone(ư, u, mod_horn, tone_ngang, 'w').
vowel_nucleus_mod_tone(ừ, u, mod_horn, tone_huyen, 'w').
vowel_nucleus_mod_tone(ứ, u, mod_horn, tone_sac, 'w').
vowel_nucleus_mod_tone(ử, u, mod_horn, tone_hoi, 'w').
vowel_nucleus_mod_tone(ữ, u, mod_horn, tone_nga, 'w').
vowel_nucleus_mod_tone(ự, u, mod_horn, tone_nang, 'w').

vowel_nucleus_mod_tone(y, y, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ỳ, y, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(ý, y, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ỷ, y, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ỹ, y, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ỵ, y, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ai, ai, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ài, ai, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(ái, ai, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ải, ai, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ãi, ai, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ại, ai, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ao, ao, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ào, ao, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(áo, ao, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ảo, ao, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ão, ao, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ạo, ao, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(au, au, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(au, au, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(au, au, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(au, au, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(au, au, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(au, au, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ay, ay, mod_none, tone_ngang, '').

vowel_nucleus_mod_tone(eo, eo, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(èo, eo, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(éo, eo, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(ẻo, eo, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(ẽo, eo, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(ẹo, eo, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(ia, ia, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(iu, iu, mod_none, tone_ngang, '').

vowel_nucleus_mod_tone(iê, ie, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(iề, ie, mod_hat, tone_huyen, '').
vowel_nucleus_mod_tone(iế, ie, mod_hat, tone_sac, '').
vowel_nucleus_mod_tone(iể, ie, mod_hat, tone_hoi, '').
vowel_nucleus_mod_tone(iễ, ie, mod_hat, tone_nga, '').
vowel_nucleus_mod_tone(iệ, ie, mod_hat, tone_nang, '').

vowel_nucleus_mod_tone(iêu, ieu, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(oi, oi, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(ua, ua, mod_horn, tone_ngang, '').
vowel_nucleus_mod_tone(ui, ui, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(uyê, uye, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(uô, uo, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(uôi, uoi, mod_horn, tone_ngang, '').

vowel_nucleus_mod_tone(uy, uy, mod_none, tone_ngang, '').
vowel_nucleus_mod_tone(uỳ, uy, mod_none, tone_huyen, '').
vowel_nucleus_mod_tone(uý, uy, mod_none, tone_sac, '').
vowel_nucleus_mod_tone(uỷ, uy, mod_none, tone_hoi, '').
vowel_nucleus_mod_tone(uỹ, uy, mod_none, tone_nga, '').
vowel_nucleus_mod_tone(uỵ, uy, mod_none, tone_nang, '').

vowel_nucleus_mod_tone(âu, au, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(ây, ay, mod_hat, tone_ngang, '').
vowel_nucleus_mod_tone(êu, eu, mod_hat, tone_ngang, '').

vowel_nucleus_mod_tone(ôi, oi, mod_hat, tone_ngang, 'o').
vowel_nucleus_mod_tone(ồi, oi, mod_hat, tone_huyen, 'o').
vowel_nucleus_mod_tone(ối, oi, mod_hat, tone_sac, 'o').
vowel_nucleus_mod_tone(ổi, oi, mod_hat, tone_hoi, 'o').
vowel_nucleus_mod_tone(ỗi, oi, mod_hat, tone_nga, 'o').
vowel_nucleus_mod_tone(ội, oi, mod_hat, tone_nang, 'o').

vowel_nucleus_mod_tone(ưa, ua, mod_horn, tone_ngang, '').
vowel_nucleus_mod_tone(ơi, oi, mod_horn, tone_ngang, '').
vowel_nucleus_mod_tone(ưi, ui, mod_horn, tone_ngang, '').
vowel_nucleus_mod_tone(ưu, uu, mod_horn, tone_ngang, '').

vowel_nucleus_mod_tone(ươ, uo, mod_horn, tone_ngang, 'w').
vowel_nucleus_mod_tone(ườ, uo, mod_horn, tone_huyen, 'w').
vowel_nucleus_mod_tone(ướ, uo, mod_horn, tone_sac, 'w').
vowel_nucleus_mod_tone(ưở, uo, mod_horn, tone_hoi, 'w').
vowel_nucleus_mod_tone(ưỡ, uo, mod_horn, tone_nga, 'w').
vowel_nucleus_mod_tone(ượ, uo, mod_horn, tone_nang, 'w').

vowel_nucleus_mod_tone(ươi, uoi, mod_horn, tone_ngang, '').
vowel_nucleus_mod_tone(ươu, uou, mod_horn, tone_ngang, '').

vowel_nucleus_mod_tone(V, V, mod_none, tone_ngang, '').
