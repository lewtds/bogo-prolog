dat_dau(Input, Mod, Tone, Output) :-
    % break the syllable down into initial consonant, vowel nucleus and final consonant
    once(phrase(syllable(I, V, F), Input)),
    % clear the tone and mark
    vowel_nucleus_mod_tone(V, V_raw, _, _),
    % add the intended ones
    vowel_nucleus_mod_tone(V_out, V_raw, Mod, Tone),
    atomic_list_concat([I, V_out, F], Output).

syllable(I, V, F) --> coni(I), vowel(V), conf(F).

vowel(V) --> vowel_chars(Vs), { atom_chars(V, Vs) }.

vowel_chars([X|Rest]) --> vowel_char(X), vowel_chars(Rest).
vowel_chars([]) --> [].

vowel_char(C) --> [C], {
    atom_chars(àáảãạaằắẳẵặăầấẩẫậâèéẻẽẹeềếểễệêìíỉĩịiòóỏõọoồốổỗộôờớởỡợơùúủũụuừứửữựưỳýỷỹỵy, AtomList),
    member(C, AtomList)
}.

coni('') --> [].
coni(m) --> [m].
coni(n) --> [n].
coni(nh) --> [n, h].
coni(ng) --> [n, g].
coni(ngh) --> [n, g, h].
coni(p) --> [p].
coni(t) --> [t].
coni(ch) --> [c, h].
coni(c) --> [c].
coni(k) --> [k].
coni(qu) --> [q, u].
coni(ph) --> [p, h].
coni(th) --> [t, h].
coni(kh) --> [k, h].
coni(b) --> [b].
coni(đ) --> [đ].
coni(s) --> [s].
coni(x) --> [x].
coni(h) --> [h].
coni(d) --> [d].
coni(gi) --> [g, i].
coni(g) --> [g].
coni(gh) --> [g, h].
coni(v) --> [v].
coni(l) --> [l].
coni(r) --> [r].

conf('') --> [].
conf(n) --> [n].
conf(nh) --> [n, h].
conf(ng) --> [n, g].
conf(c) --> [c].
conf(ch) --> [c, h].
conf(p) --> [p].
conf(t) --> [t].
conf(n) --> [n].
conf(m) --> [m].

vowel_nucleus_mod_tone(a, a, mod_none, tone_ngang).
vowel_nucleus_mod_tone(à, a, mod_none, tone_huyen).
vowel_nucleus_mod_tone(á, a, mod_none, tone_sac).
vowel_nucleus_mod_tone(ả, a, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ã, a, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạ, a, mod_none, tone_nang).

vowel_nucleus_mod_tone(â, a, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(ầ, a, mod_hat, tone_huyen).
vowel_nucleus_mod_tone(ấ, a, mod_hat, tone_sac).
vowel_nucleus_mod_tone(ẩ, a, mod_hat, tone_hoi).
vowel_nucleus_mod_tone(ẫ, a, mod_hat, tone_nga).
vowel_nucleus_mod_tone(ậ, a, mod_hat, tone_nang).

vowel_nucleus_mod_tone(ă, a, mod_breve, tone_ngang).
vowel_nucleus_mod_tone(ằ, a, mod_breve, tone_huyen).
vowel_nucleus_mod_tone(ắ, a, mod_breve, tone_sac).
vowel_nucleus_mod_tone(ẳ, a, mod_breve, tone_hoi).
vowel_nucleus_mod_tone(ẵ, a, mod_breve, tone_nga).
vowel_nucleus_mod_tone(ặ, a, mod_breve, tone_nang).

vowel_nucleus_mod_tone(ai, ai, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ài, ai, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ái, ai, mod_none, tone_sac).
vowel_nucleus_mod_tone(ải, ai, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ãi, ai, mod_none, tone_nga).
vowel_nucleus_mod_tone(ại, ai, mod_none, tone_nang).

vowel_nucleus_mod_tone(ao, ao, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ào, ao, mod_none, tone_huyen).
vowel_nucleus_mod_tone(áo, ao, mod_none, tone_sac).
vowel_nucleus_mod_tone(ảo, ao, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ão, ao, mod_none, tone_nga).
vowel_nucleus_mod_tone(ạo, ao, mod_none, tone_nang).

vowel_nucleus_mod_tone(au, au, mod_none, tone_ngang).
vowel_nucleus_mod_tone(au, au, mod_none, tone_huyen).
vowel_nucleus_mod_tone(au, au, mod_none, tone_sac).
vowel_nucleus_mod_tone(au, au, mod_none, tone_hoi).
vowel_nucleus_mod_tone(au, au, mod_none, tone_nga).
vowel_nucleus_mod_tone(au, au, mod_none, tone_nang).

vowel_nucleus_mod_tone(ay, ay, mod_none, tone_ngang).
vowel_nucleus_mod_tone(eo, eo, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ia, ia, mod_none, tone_ngang).
vowel_nucleus_mod_tone(iu, iu, mod_none, tone_ngang).

vowel_nucleus_mod_tone(iê, ie, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(iề, ie, mod_hat, tone_huyen).
vowel_nucleus_mod_tone(iế, ie, mod_hat, tone_sac).
vowel_nucleus_mod_tone(iể, ie, mod_hat, tone_hoi).
vowel_nucleus_mod_tone(iễ, ie, mod_hat, tone_nga).
vowel_nucleus_mod_tone(iệ, ie, mod_hat, tone_nang).

vowel_nucleus_mod_tone(iêu, ieu, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(oi, oi, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ua, ua, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ui, ui, mod_none, tone_ngang).
vowel_nucleus_mod_tone(uyê, uye, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(uô, uo, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(uôi, uoi, mod_horn, tone_ngang).

vowel_nucleus_mod_tone(uy, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uỳ, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uý, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uỷ, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uỹ, uy, mod_none, tone_sac).
vowel_nucleus_mod_tone(uỵ, uy, mod_none, tone_sac).

vowel_nucleus_mod_tone(á, a, mod_none, tone_sac).
vowel_nucleus_mod_tone(âu, au, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(ây, ay, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(êu, eu, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(ôi, oi, mod_hat, tone_ngang).
vowel_nucleus_mod_tone(ắ, a, mod_breve, tone_sac).
vowel_nucleus_mod_tone(ưa, ua, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ơi, oi, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ưi, ui, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ưu, uu, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ươ, uo, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ươi, uoi, mod_horn, tone_ngang).
vowel_nucleus_mod_tone(ươu, uou, mod_horn, tone_ngang).

vowel_nucleus_mod_tone(y, y, mod_none, tone_ngang).
vowel_nucleus_mod_tone(ỳ, y, mod_none, tone_huyen).
vowel_nucleus_mod_tone(ý, y, mod_none, tone_sac).
vowel_nucleus_mod_tone(ỷ, y, mod_none, tone_hoi).
vowel_nucleus_mod_tone(ỹ, y, mod_none, tone_nga).
vowel_nucleus_mod_tone(ỵ, y, mod_none, tone_nang).

vowel_nucleus_mod_tone(V, V, mod_none, tone_ngang).
