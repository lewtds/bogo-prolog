:- set_prolog_flag(encoding, utf8).

% Needs SWI-Prolog version >= 8.1.13 for with_tty_raw
%
% Usage example:
% ?- consult('bogo.prolog').
% ?- interactive.
%
% Then start typing.
%
% [t],t
% [t,r],tr
% [t,r,u],tru
% [t,r,u,o],truo
% [t,r,u,o,w],trươ
% [t,r,u,o,w,n],trươn
% [t,r,u,o,w,n,g],trương
% [t,r,u,o,w,n,g,f],trường
% [h],h
% [h,o],ho
% [h,o,p],hop
% [h,o,p,w],hơp
% [h,o,p,w,j],hợp
interactive :- with_tty_raw(loop_process_key([])).

:- set_flag(bogo:dau_moi, true).

% food for thought:
% gìn giữ
% giặt gịa
% giặt gyạ (archaic)
% giặt gỵa
% context sensitive grammar?

loop_process_key(Sequence) :-
    get_char(C),
    (   C \= ' ',
        append(Sequence, [C], Sequence2),
        process_key_sequence(Sequence2, [], OutCharList),
        atom_chars(Atom, OutCharList),
        writeln((Sequence2, Atom)),
        loop_process_key(Sequence2)
    ;   loop_process_key([]) % start a new sequence with space character
    ).

% TODO: Upper case support

% TELEX
key_effect(f, add_tone(tone_huyen)).
key_effect(s, add_tone(tone_sac)).
key_effect(r, add_tone(tone_hoi)).
key_effect(x, add_tone(tone_nga)).
key_effect(j, add_tone(tone_nang)).

key_effect(a, add_vowel_mod(mod_hat_a)).
key_effect(e, add_vowel_mod(mod_hat_e)).
key_effect(o, add_vowel_mod(mod_hat_o)).
key_effect(w, add_vowel_mod(mod_horn_uo)).
key_effect(w, add_vowel_mod(mod_horn_o)).
key_effect(w, add_vowel_mod(mod_horn_u)).
key_effect(w, add_vowel_mod(mod_breve_a)).

key_effect(d, add_consonant_mod(mod_dash_d)).

% VNI
key_effect('1', add_tone(tone_sac)).
key_effect('2', add_tone(tone_huyen)).
key_effect('3', add_tone(tone_hoi)).
key_effect('4', add_tone(tone_nga)).
key_effect('5', add_tone(tone_nang)).

key_effect('6', add_vowel_mod(mod_hat_a)).
key_effect('6', add_vowel_mod(mod_hat_e)).
key_effect('6', add_vowel_mod(mod_hat_o)).
key_effect('7', add_vowel_mod(mod_horn_uo)).
key_effect('7', add_vowel_mod(mod_horn_o)).
key_effect('7', add_vowel_mod(mod_horn_u)).
key_effect('8', add_vowel_mod(mod_breve_a)).

key_effect('9', add_consonant_mod(mod_dash_d)).

% Usage example:
%
% ?- process_key_sequence([b, a, n, s, w], [] , Output).
%    Output = [b, ắ, n].
%

process_atom(AtomIn, OutList) :-
    atom_chars(AtomIn, Sequence),
    process_key_sequence(Sequence, OutList).

process_key_sequence(Sequence, Output) :-
    process_key_sequence(Sequence, [], Output).

process_key_sequence([Key|Rest], CurrentString, Output) :-
    once(process_key(CurrentString, Key, Output1)),
    process_key_sequence(Rest, Output1, Output).

process_key_sequence([], CurrentString, CurrentString).

% fall back to the raw sequence if processing fails (vowel -> vơel)
process_key_sequence(Keys, [], Keys).

% Try applying the effect of the key, assuming that there is one.
% Performance optimization idea: give out both an output string and a syllable term and reuse the term for the next cycle.
process_key(CurrentString, Key, OutString) :-
    % break the syllable down into initial consonant, vowel nucleus and final consonant
    once(phrase(syllable(I, V, F), CurrentString)),

    apply_key_effect(s(I, V, F), Key, Out),
    rebalance_incomplete_form(Out, CleanedSyllable),

    syllable_atom(CleanedSyllable, OutputAtom),
    atom_chars(OutputAtom, OutString),

    % require that the output must be parsable, this is to weed out non-Vietnamese words like vowel -> vơel
    once(phrase(syllable(_, _, _), OutString)).

syllable_atom(s(I, V, F), Atom) :- atomic_list_concat([I, V, F], Atom).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_tone(Tone)),
    syllable_tone(InSyllable, Tone, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_vowel_mod(Mod)),
    syllable_vowel_mod(InSyllable, Mod, OutSyllable).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_consonant_mod(Mod)),
    syllable_consonant_mod(InSyllable, Mod, OutSyllable).

% If none of the transformation rules apply, append the key to the end of the syllable
apply_key_effect(s(I, V, ''), Key, s(I, V2, '')) :- vowel_char(Key, [Key], []), atom_concat(V, Key, V2).
apply_key_effect(s(I, '', ''), Key, s(I2, '', '')) :- atom_concat(I, Key, I2).
apply_key_effect(s(I, V, F), Key, s(I, V, F2)) :- atom_concat(F, Key, F2).

syllable_vowel_mod(s(I, V, F), Mod, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(s(I, V, F), s(I, V_raw, F), _, Tone),
    vowel_nucleus_mod_tone(s(I, V_out, F), s(I, V_raw, F), Mod, Tone).

syllable_tone(s(I, V, F), Tone, s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(s(I, V, F), s(I, V_raw, F), Mod, _),
    vowel_nucleus_mod_tone(s(I, V_out, F), s(I, V_raw, F), Mod, Tone).

% Eg: to turn thuơng into thương, mịen to miẹn
rebalance_incomplete_form(s(I, V, F), s(I, V_out, F)) :-
    vowel_nucleus_mod_tone(s(I, V, F), s(I, V_raw, F), Mod, Tone),
    vowel_nucleus_mod_tone(s(I, V_out, F), s(I, V_raw, F), Mod, Tone).

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
consonant_initial(q) --> [q]. % I'm a bit iffy on this case since it allows typing teencode-looking words like qán, qẩn
consonant_initial(ph) --> [p, h].
consonant_initial(th) --> [t, h].
consonant_initial(kh) --> [k, h].
consonant_initial(b) --> [b].
consonant_initial(đ) --> [đ].
consonant_initial(s) --> [s].
consonant_initial(x) --> [x].
consonant_initial(h) --> [h].
consonant_initial(d) --> [d].
% già
consonant_initial(gi), [V] --> [g, i], vowel(V).
% gì, gà
consonant_initial(g) --> [g].
consonant_initial(gh) --> [g, h].
consonant_initial(v) --> [v].
consonant_initial(l) --> [l].
consonant_initial(r) --> [r].
% for the dzũng zũng people
consonant_initial(dz) --> [d, z].
consonant_initial(z) --> [z].

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

% vowel nucleus transformation table schema:
%   resulting syllable, base syllable, vowel mod, tone, allow incomplete
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_ngang) :- terminal_vowel(Raw, Mod, (Result, _, _, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_huyen) :- terminal_vowel(Raw, Mod, (_, Result, _, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_sac)   :- terminal_vowel(Raw, Mod, (_, _, Result, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_hoi)   :- terminal_vowel(Raw, Mod, (_, _, _, Result, _, _)).
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_nga)   :- terminal_vowel(Raw, Mod, (_, _, _, _, Result, _)).
vowel_nucleus_mod_tone(s(I, Result, ''), s(I, Raw, ''), Mod, tone_nang)  :- terminal_vowel(Raw, Mod, (_, _, _, _, _, Result)).

vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_ngang) :- not_strictly_terminal_vowel(Raw, Mod, (Result, _, _, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_huyen) :- not_strictly_terminal_vowel(Raw, Mod, (_, Result, _, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_sac)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, Result, _, _, _)).
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_hoi)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, Result, _, _)).
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_nga)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, _, Result, _)).
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_nang)  :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, _, _, Result)).

vowel_nucleus_mod_tone(s(I, V, F), s(I, V, F), mod_none, tone_ngang).

% not_strictly_terminal_vowel means the vowel can be both terminal and non-terminal.
% Would it be useful to also distinguish strictly non-terminal vowels?
not_strictly_terminal_vowel(a, mod_breve_a, (ă, ằ, ắ, ẳ, ẵ, ặ)).
not_strictly_terminal_vowel(a, mod_hat_a, (â, ầ, ấ, ẩ, ẫ, ậ)).
not_strictly_terminal_vowel(a, mod_none, (a, à, á, ả, ã, ạ)).
not_strictly_terminal_vowel(e, mod_hat_e, (ê, ề, ế, ể, ễ, ệ)).
not_strictly_terminal_vowel(e, mod_none, (e, è, é, ẻ, ẽ, ẹ)).
not_strictly_terminal_vowel(i, mod_none, (i, ì, í, ỉ, ĩ, ị)).
not_strictly_terminal_vowel(ie, mod_hat_e, (iê, iề, iế, iể, iễ, iệ)).
not_strictly_terminal_vowel(o, mod_hat_o, (ô, ồ, ố, ổ, ỗ, ộ)).
not_strictly_terminal_vowel(o, mod_horn_o, (ơ, ờ, ớ, ở, ỡ, ợ)).
not_strictly_terminal_vowel(o, mod_none, (o, ò, ó, ỏ, õ, ọ)).
not_strictly_terminal_vowel(oa, mod_breve_a, (oă, oằ, oắ, oẳ, oẵ, oặ)).
not_strictly_terminal_vowel(oa, mod_none, (oa, oà, oá, oả, oã, oạ)).
not_strictly_terminal_vowel(u, mod_horn_u, (ư, ừ, ứ, ử, ữ, ự)).
not_strictly_terminal_vowel(u, mod_none, (u, ù, ú, ủ, ũ, ụ)).
not_strictly_terminal_vowel(ue, mod_hat_e, (uê, uề, uế, uể, uễ, uệ)).
not_strictly_terminal_vowel(ua, mod_hat_a, (uâ, uầ, uấ, uẩ, uẫ, uậ)).
not_strictly_terminal_vowel(uo, mod_hat_o, (uô, uồ, uố, uổ, uỗ, uộ)).
not_strictly_terminal_vowel(uo, mod_horn_uo, (ươ, ườ, ướ, ưở, ưỡ, ượ)).
not_strictly_terminal_vowel(uo, mod_horn_uo, (uơ, uờ, uớ, uở, uỡ, uợ)).
% huyền, thuyền
not_strictly_terminal_vowel(uye, mod_hat_e, (uyê, uyề, uyế, uyể, uyễ, uyệ)).
% quyền and yến
not_strictly_terminal_vowel(ye, mod_hat_e, (yê, yề, yế, yể, yễ, yệ)).

% The incomplete forms happen as transitional states before the final word. For example, if you type huyenf then you get
% huyèn, an arguably invalid VNmese word, but if you type 'e' after that, you get huyền, a valid word.
% We don't do anything with the incomplete forms yet but it's a good distintion to make, at least at the mental level.
not_strictly_terminal_vowel(A, B, C) :- not_strictly_terminal_vowel_incomplete(A, B, C).

not_strictly_terminal_vowel_incomplete(uo, mod_none, (uo, uò, uó, uỏ, uõ, uọ)).
not_strictly_terminal_vowel_incomplete(uo, mod_horn_uo, (ưo, ừo, ứo, ửo, ữo, ựo)).
not_strictly_terminal_vowel_incomplete(uo, mod_horn_uo, (ưo, ưò, ưó, ưỏ, ưõ, ưọ)).
not_strictly_terminal_vowel_incomplete(uye, mod_none, (uye, uyè, uyé, uyẻ, uyẽ, uyẹ)).
not_strictly_terminal_vowel_incomplete(ie, mod_none, (ie, iè, ié, iẻ, iẽ, iẹ)).
not_strictly_terminal_vowel_incomplete(ie, mod_none, (ie, ìe, íe, ỉe, ĩe, ịe)).
not_strictly_terminal_vowel_incomplete(ue, mod_none, (ue, uè, ué, uẻ, uẽ, uẹ)).

terminal_vowel(ai, mod_none, (ai, ài, ái, ải, ãi, ại)).
terminal_vowel(ao, mod_none, (ao, ào, áo, ảo, ão, ạo)).
terminal_vowel(au, mod_hat_a, (âu, ầu, ấu, ẩu, ẫu, ậu)).
terminal_vowel(au, mod_none, (au, àu, áu, ảu, ãu, ạu)).
terminal_vowel(ay, mod_hat_a, (ây, ầy, ấy, ẩy, ẫy, ậy)).
terminal_vowel(ay, mod_none, (ay, ày, áy, ảy, ãy, ạy)).
terminal_vowel(eo, mod_none, (eo, èo, éo, ẻo, ẽo, ẹo)).
terminal_vowel(eu, mod_hat_e, (êu, ều, ếu, ểu, ễu, ệu)).
terminal_vowel(ia, mod_none, (ia, ìa, ía, ỉa, ĩa, ịa)).
terminal_vowel(ie, mod_none, (ie, ìe, íe, ỉe, ĩe, ịe)).
terminal_vowel(ieu, mod_hat_e, (iêu, iều, iếu, iểu, iễu, iệu)).
terminal_vowel(iu, mod_none, (iu, ìu, íu, ỉu, ĩu, ịu)).
terminal_vowel(oa, mod_none, (oa, oà, oá, oả, oã, oạ)) :- get_flag(bogo:dau_moi, true).
terminal_vowel(oa, mod_none, (oa, òa, óa, ỏa, õa, ọa)).
terminal_vowel(oe, mod_none, (oe, oè, oé, oẻ, oẽ, oẹ)) :- get_flag(bogo:dau_moi, true).
terminal_vowel(oe, mod_none, (oe, òe, óe, ỏe, õe, ọe)).
terminal_vowel(oi, mod_none, (oi, òi, ói, ỏi, õi, ọi)).
terminal_vowel(ua, mod_none, (ua, ùa, úa, ủa, ũa, ụa)).
terminal_vowel(ui, mod_horn_u, (ưi, ừi, ứi, ửi, ữi, ựi)).
terminal_vowel(ui, mod_none, (ui, ùi, úi, ủi, ũi, ụi)).
terminal_vowel(uo, mod_horn_uo, (uơ, uờ, uớ, uở, uỡ, uợ)).
terminal_vowel(uoi, mod_hat_o, (uôi, uồi, uối, uổi, uỗi, uội)).
terminal_vowel(uoi, mod_horn_uo, (ươi, ười, ưới, ưởi, ưỡi, ượi)).
terminal_vowel(uou, mod_horn_uo, (ươu, ườu, ướu, ưởu, ưỡu, ượu)).
terminal_vowel(uu, mod_horn_u, (ưu, ừu, ứu, ửu, ữu, ựu)).
terminal_vowel(uy, mod_none, (uy, uỳ, uý, uỷ, uỹ, uỵ)) :- get_flag(bogo:dau_moi, true).
terminal_vowel(uy, mod_none, (uy, ùy, úy, ủy, ũy, ụy)).
terminal_vowel(y, mod_none, (y, ỳ, ý, ỷ, ỹ, ỵ)).
terminal_vowel(ya, mod_none, (ya, ỳa, ýa, ỷa, ỹa, ỵa)).

terminal_vowel(A, B, C) :- terminal_vowel_incomplete(A, B, C).

terminal_vowel_incomplete(uoi, mod_none, (uoi, uòi, uói, uỏi, uõi, uọi)).
terminal_vowel_incomplete(uoi, mod_none, (uoi, uòi, uói, uỏi, uõi, uọi)).
terminal_vowel_incomplete(uoi, mod_horn_uo, (uơi, uời, uới, uởi, uỡi, uợi)).
terminal_vowel_incomplete(uou, mod_none, (uou, uòu, uóu, uỏu, uõu, uọu)).
