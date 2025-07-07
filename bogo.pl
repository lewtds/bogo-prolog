:- set_prolog_flag(encoding, utf8).
:- include(test_cases).
:- use_module(library(dcg/high_order)).

% The main idea of this Vietnamese input method engine is that of a syllable parser. A Vietnamese syllable (âm tiết)
% consists of an onset (phụ âm đầu), a rhyme (vần) and a tone (thanh điệu). The rhyme itself consists of a 
% nuclei (âm chính), a final (âm cuối), which could be either a consonant or a vowel. For example the syllable "nhanh" is parsed as followed:
% 
% ?- phrase(syllable(Onset, Nuclei, Final, Tone), [n, h, a, n, h]).
% Onset = Final, Final = nh,
% Nuclei = a,
% Tone = tone_blank
% 
% Similarly, "nhai" is parsed as:
% 
% ?- phrase(syllable(Onset, Nuclei, Final, Tone), [n, h, a, i]).
% Onset = nh,
% Nuclei = a,
% Final = i,
% Tone = tone_blank 
% 
% Because this is Prolog, the parser can work in generator mode as well so we can produce the text for a syllable given its components.
% For example, to change the tone of "nhai" to acute, we can do the following:
% 
% ?- phrase(syllable(nh, a, i, tone_acute), Text).
% Text = [n, h, á, i] 
% 
% The input method engine contains a database of legal combinations [1] and will parse the input keys and apply the 
% appropriate modifications to the syllable.
% 
% [1]: https://www.hieuthi.com/blog/2017/03/21/all-vietnamese-syllables.html

% TELEX
key_effect(f, add_tone(tone_grave)).
key_effect(s, add_tone(tone_acute)).
key_effect(r, add_tone(tone_hook)).
key_effect(x, add_tone(tone_tilde)).
key_effect(j, add_tone(tone_dot)).

key_effect(a, add_nuclei_mod(mod_hat_a)).
key_effect(e, add_nuclei_mod(mod_hat_e)).
key_effect(o, add_nuclei_mod(mod_hat_o)).

key_effect(o, add_nuclei_mod(mod_oo)).

key_effect(w, add_nuclei_mod(mod_breve_a)).

key_effect(w, add_nuclei_mod(mod_horn_uo)).
key_effect(w, add_nuclei_mod(mod_horn_o)).
key_effect(w, add_nuclei_mod(mod_horn_u)).
key_effect(o, add_nuclei_mod(mod_complete_horn_uo)).

key_effect(d, add_onset_mod(mod_dash_d)).

process_atom(InputAtom, OutputAtom) :-
    atom_chars(InputAtom, Keys),
    process_key_sequence(Keys, OutString),
    atom_chars(OutputAtom, OutString).

process_key_sequence(Keys, OutString) :-
    foldl(process_key, Keys, [], OutString), !.

% If the key sequence cannot be processed, return the keys as is.
process_key_sequence(Keys, Keys).

process_key(Key, InAtomList, OutAtomList) :-
    process_key_(Key, InAtomList, OutSyllable),
    post_procress_syllable(OutSyllable, syllable(O, N, F, T)),
    phrase(syllable(O, N, F, T), OutAtomList).

% Try to apply a key effect to the current syllable.
process_key_(Key, InAtomList, OutSyllable) :-
    phrase(syllable(O, N, F, T), InAtomList),
    \+ (O = '', N = '', F = '', T = tone_blank), % Ensure the syllable is not empty.
    apply_key_effect(syllable(O, N, F, T), Key, OutSyllable), !.

% If the key effect cannot be applied, just append the key to the current string.
process_key_(Key, CurrentString, syllable(O, N, F, T)) :-
    append(CurrentString, [Key], OutString),
    % Require the result to be a valid syllable.
    phrase(syllable(O, N, F, T), OutString).

post_procress_syllable(syllable(Onset, ươ, '', Tone), syllable(Onset, uơ, '', Tone)) :- (
    Onset = 'th';
    Onset = 'h'
).
post_procress_syllable(syllable(Onset, uơ, Final, Tone), syllable(Onset, ươ, Final, Tone)) :- Final \= '', !.
post_procress_syllable(Bypass, Bypass).

% Replace the tone
apply_key_effect(syllable(Onset, Nuclei, Final, Tone), Key, syllable(Onset, Nuclei, Final, NewTone)) :-
    key_effect(Key, add_tone(NewTone)),
    Tone \= NewTone.

% Add a nuclei modification
apply_key_effect(syllable(Onset, Nuclei, Final, Tone), Key, syllable(Onset, NucleiModded, Final, Tone)) :-
    key_effect(Key, add_nuclei_mod(Mod)),
    nuclei_mod(Mod, Nuclei, NucleiModded).

% Add an onset modification
apply_key_effect(syllable(d, Nuclei, Final, Tone), Key, syllable(đ, Nuclei, Final, Tone)) :-
    key_effect(Key, add_onset_mod(mod_dash_d)).

%%
%% Syllable Parser
%%

% An empty syllable
syllable('', '', '', tone_blank) --> [].

% A syllable with only an onset
syllable(O, '', '', tone_blank) --> onset(O), { O \= '' }.

% gi and qu are only allowed to be onset if they are followed by a rhyme
syllable(gi, N, F, T) --> [g, i], rhyme(N, F, T), {
    N \= 'ye', N \= 'yê'
}.
syllable(qu, N, F, T) --> [q, u], rhyme(N, F, T), {
    N \= 'ye', N \= 'yê'
}.

% A syllable with only a rhyme
syllable('', N, F, T) --> rhyme(N, F, T).

% A "normal" syllable with both an onset and a rhyme
syllable(O, N, F, T) --> onset(O), rhyme(N, F, T), {
    N \= 'ye', N \= 'yê'
}.

% ngh, gh and k can either standalone with no rhyme or be followed by i, e, ê, ie, iê
syllable(k, '', '', tone_blank) --> [k].
syllable(gh, '', '', tone_blank) --> [g, h].
syllable(ngh, '', '', tone_blank) --> [n, g, h].
syllable(ngh, N, F, T) --> [n, g, h], rhyme(N, F, T), { member(N, [i, e, ê, ie, iê]) }.
syllable(gh, N, F, T) --> [g, h], rhyme(N, F, T), { member(N, [i, e, ê, ie, iê]) }.
syllable(k, N, F, T) --> [k], rhyme(N, F, T), { member(N, [i, y, e, ê, ie, iê]) }.

onset(O) --> { member(O, [
    b, d, h, l, m, n, p, r, s, t, v, q, x, z, đ,
    tr, th, ch, ph, nh, kh,
    ng, g, c
]), atom_chars(O, Os) }, Os.


rhyme(Nuclei, Final, Tone) --> {
    rhyme_with_tone(RWTones, Nuclei, Final, Tone),
    atom_chars(RWTones, Rs)
}, Rs.

% rhyme_with_tone unpacks the legal rhymes DB below and relate a rhyme to its nuclei, final and tone.
% eg. rhyme_with_tone(ươ, ng, tone_grave, ường).
rhyme_with_tone(Rhyme, NucleiNoTone, Final, Tone) :-
    (
        unconstrained_rhyme([Nuclei, _, _, _, _, _], Finals), NucleiNoTone = Nuclei, Tone = tone_blank;
        unconstrained_rhyme([NucleiNoTone, Nuclei, _, _, _, _], Finals), Tone = tone_grave;
        unconstrained_rhyme([NucleiNoTone, _, Nuclei, _, _, _], Finals), Tone = tone_acute;
        unconstrained_rhyme([NucleiNoTone, _, _, Nuclei, _, _], Finals), Tone = tone_hook;
        unconstrained_rhyme([NucleiNoTone, _, _, _, Nuclei, _], Finals), Tone = tone_tilde;
        unconstrained_rhyme([NucleiNoTone, _, _, _, _, Nuclei], Finals), Tone = tone_dot;

        kpt_rhyme([Nuclei, _, _], Finals), NucleiNoTone = Nuclei, Tone = tone_blank;
        kpt_rhyme([NucleiNoTone, Nuclei, _], Finals), Tone = tone_acute;
        kpt_rhyme([NucleiNoTone, _, Nuclei], Finals), Tone = tone_dot
    ),
    member(Final, Finals),
    atom_concat(Nuclei, Final, Rhyme).

% Unconstrained rhymes are those that can be used with any tone.
% KPT rhymes are those that can only be used with the acute or dot tone.
:- discontiguous unconstrained_rhyme/2.
:- discontiguous kpt_rhyme/2.

unconstrained_rhyme([a, à, á, ả, ã, ạ], ['', i, o, u, y, m, n, ng, nh]).
kpt_rhyme([a, á, ạ], [c, ch, t, p]).

unconstrained_rhyme([ă, ằ, ắ, ẳ, ẵ, ặ], ['', m, n, ng]).
kpt_rhyme([ă, ắ, ặ], [c, t, p]).

unconstrained_rhyme([â, ầ, ấ, ẩ, ẫ, ậ], ['', u, y, m, n, ng]).
kpt_rhyme([â, ấ, ậ], [c, t, p]).

unconstrained_rhyme([â, ầ, ấ, ẩ, ẫ, ậ], ['', u, y, m, n, ng]).
kpt_rhyme([â, ấ, ậ], [c, t, p]).

unconstrained_rhyme([e, è, é, ẻ, ẽ, ẹ], ['', o, u, m, n, ng, nh]).
kpt_rhyme([e, é, ẹ], [c, ch, t, p]).

unconstrained_rhyme([ê, ề, ế, ể, ễ, ệ], ['', u, m, n, ng, nh]).
kpt_rhyme([ê, ế, ệ], [c, ch, t, p]).

unconstrained_rhyme([ê, ề, ế, ể, ễ, ệ], ['', u, m, n, ng]).
kpt_rhyme([ê, ế, ệ], [c, ch, t, p]).

unconstrained_rhyme([i, ì, í, ỉ, ĩ, ị], ['', a, u, m, n, nh]).
kpt_rhyme([i, í, ị], [c, ch, t, p]).

unconstrained_rhyme([o, ò, ó, ỏ, õ, ọ], ['', i, m, n, ng]).
kpt_rhyme([o, ó, ọ], [c, t, p]).

unconstrained_rhyme([ô, ồ, ố, ổ, ỗ, ộ], ['', i, m, n, ng]).
kpt_rhyme([ô, ố, ộ], [c, t, p]).

unconstrained_rhyme([ơ, ờ, ớ, ở, ỡ, ợ], ['', i, m, n]).
kpt_rhyme([ơ, ớ, ợ], [t, p]).

unconstrained_rhyme([u, ù, ú, ủ, ũ, ụ], ['', a, i, u, m, n, ng]).
kpt_rhyme([u, ú, ụ], [c, t, p]).

unconstrained_rhyme([ư, ừ, ứ, ử, ữ, ự], ['', a, i, u, m, n, ng]).
kpt_rhyme([ư, ứ, ự], [c, t]).

unconstrained_rhyme([y, ỳ, ý, ỷ, ỹ, ỵ], ['']).

unconstrained_rhyme([ie, iè, ié, iẻ, iẽ, iẹ], ['', u, m, n, ng]).
kpt_rhyme([ie, ié, iẹ], [c, t, p]).

unconstrained_rhyme([iê, iề, iế, iể, iễ, iệ], ['', u, m, n, ng]).
kpt_rhyme([iê, iế, iệ], [c, t, p]).

unconstrained_rhyme([oa, oà, oá, oả, oã, oạ], [i, o, y, m, n, ng, nh]).
kpt_rhyme([oa, oá, oạ], [c, ch, t, p]).

unconstrained_rhyme([oa, òa, óa, ỏa, õa, ọa], ['', i, o, y, m, n, ng, nh]).
kpt_rhyme([oa, óa, ọa], [c, ch, t, p]).

unconstrained_rhyme([oă, oằ, oắ, oẳ, oẵ, oặ], ['', i, o, y, m, n, ng, nh]).
kpt_rhyme([oă, oắ, oặ], [c, t]).

unconstrained_rhyme([oe, oè, oé, oẻ, oẽ, oẹ], [o, m, n]).
kpt_rhyme([oe, oé, oẹ], [t]).

unconstrained_rhyme([oe, òe, óe, ỏe, õe, ọe], ['', o, m, n]).
kpt_rhyme([oe, óe, ọe], [t]).

unconstrained_rhyme([oo, oò, oó, oỏ, oõ, oọ], ['', n, ng]).
kpt_rhyme([oo, oó, oọ], [c]).

unconstrained_rhyme([ua, ùa, úa, ủa, ũa, ụa], ['', y, n, ng]).
kpt_rhyme([ua, úa, ụa], [t]).

unconstrained_rhyme([ua, uà, uá, uả, uã, uạ], ['', y, n, ng]).
kpt_rhyme([ua, uá, uạ], [t]).

unconstrained_rhyme([uâ, uầ, uấ, uẩ, uẫ, uậ], ['', y, n, ng]).
kpt_rhyme([uâ, uấ, uậ], [t]).

unconstrained_rhyme([ue, uè, ué, uẻ, uẽ, uẹ], ['', n, nh]).
kpt_rhyme([ue, ué, uẹ], [c, ch]).

unconstrained_rhyme([uê, uề, uế, uể, uễ, uệ], ['', n, nh]).
kpt_rhyme([uê, uế, uệ], [c, ch]).

unconstrained_rhyme([uo, uò, uó, uỏ, uõ, uọ], ['', i, u, m, n, ng]).
kpt_rhyme([uo, uó, uọ], [c, t, p]).

unconstrained_rhyme([uô, uồ, uố, uổ, uỗ, uộ], ['', i, m, n, ng]).
kpt_rhyme([uô, uố, uộ], [c, t, p]).

unconstrained_rhyme([uơ, uờ, uớ, uở, uỡ, uợ], ['']).

unconstrained_rhyme([uy, uỳ, uý, uỷ, uỹ, uỵ], [a, u, n, nh]).
kpt_rhyme([uy, uý, uỵ], [c, ch, t, p]).

unconstrained_rhyme([uy, ùy, úy, ủy, ũy, ụy], ['', a, u, n, nh]).
kpt_rhyme([uy, úy, ụy], [c, ch, t, p]).

unconstrained_rhyme([ươ, ườ, ướ, ưở, ưỡ, ượ], ['', i, u, m, n, ng]).
kpt_rhyme([ươ, ướ, ượ], ['', c, p, t]).

unconstrained_rhyme([uye, uyè, uyé, uyẻ, uyẽ, uyẹ], ['', n]).
kpt_rhyme([uye, uyé, uyẹ], [t]).

unconstrained_rhyme([uyê, uyề, uyế, uyể, uyễ, uyệ], ['', n]).
kpt_rhyme([uyê, uyế, uyệ], [t]).

unconstrained_rhyme([ye, yè, yé, yẻ, yẽ, yẹ], ['', u, m, n, ng]).
kpt_rhyme([ye, yé, yẹ], [t]).

unconstrained_rhyme([yê, yề, yế, yể, yễ, yệ], ['', u, m, n, ng]).
kpt_rhyme([yê, yế, yệ], [t]).

nuclei_mod(mod_hat_a, a, â).
nuclei_mod(mod_hat_a, ă, â).
nuclei_mod(mod_hat_a, â, â).
nuclei_mod(mod_hat_a, ua, uâ).

nuclei_mod(mod_hat_e, e, ê).
nuclei_mod(mod_hat_e, ie, iê).
nuclei_mod(mod_hat_e, ue, uê).
nuclei_mod(mod_hat_e, uye, uyê).
nuclei_mod(mod_hat_e, ye, yê).

nuclei_mod(mod_hat_o, o, ô).
nuclei_mod(mod_hat_o, ơ, ô).
nuclei_mod(mod_hat_o, uo, uô).

nuclei_mod(mod_breve_a, a, ă).
nuclei_mod(mod_breve_a, â, ă).
nuclei_mod(mod_breve_a, oa, oă).

nuclei_mod(mod_horn_u, u, ư).

nuclei_mod(mod_horn_o, o, ơ).
nuclei_mod(mod_horn_o, ô, ơ).

nuclei_mod(mod_horn_uo, uo, ươ).
nuclei_mod(mod_horn_uo, ưo, ươ).

nuclei_mod(mod_oo, ô, oo).

% Adding 'o' to 'ư' has the effect of creating 'ươ'
nuclei_mod(mod_complete_horn_uo, ư, ươ).

:- begin_tests(syllable_parser).

test(empty) :- phrase(syllable('', '', '', tone_blank), []).
test(consonant_only) :- 
    phrase(syllable(c, '', '', tone_blank), [c]),
    phrase(syllable(gh, '', '', tone_blank), [g, h]),
    phrase(syllable(ngh, '', '', tone_blank), [n, g, h]),
    phrase(syllable(k, '', '', tone_blank), [k]).

test(vowel_only) :- phrase(syllable('', a, '', tone_blank), [a]).
test(no_initial_consonant) :- phrase(syllable('', a, c, tone_blank), [a, c]).
test(full) :- phrase(syllable(nh, a, c, tone_blank), [n, h, a, c]).

test(gi) :- phrase(syllable(g, i, '', tone_blank), [g, i]).
test(gia) :- phrase(syllable(gi, a, '', tone_blank), [g, i, a]).
test(giam) :- phrase(syllable(gi, a, m, tone_blank), [g, i, a, m]).

test(qu) :- phrase(syllable(q, u, '', tone_blank), [q, u]).
test(qua) :- phrase(syllable(qu, a, '', tone_blank), [q, u, a]).
test(quan) :- phrase(syllable(qu, a, n, tone_blank), [q, u, a, n]).

test(quy) :- phrase(syllable(qu, y, '', tone_blank), [q, u, y]).
test(quye) :- phrase(syllable(q, uye, '', tone_blank), [q, u, y, e]).

test(r) :- phrase(syllable(r, '', '', tone_blank), [r]).

test(ye_should_not_have_onset, [fail]) :- phrase(syllable(_, _, _, tone_blank), [m, y, e]).
test(yê_should_not_have_onset, [fail]) :- phrase(syllable(_, _, _, tone_blank), [m, y, ê]).

test(extract_tone) :- phrase(syllable(nh, a, nh, tone_acute), [n, h, á, n, h]).

:- end_tests(syllable_parser).

:- begin_tests(apply_key_effect).

test(quyee) :-
    apply_key_effect(
        syllable(q, uye, '', tone_blank), e, syllable(q, uyê, '', tone_blank)).

test(rej) :-
    apply_key_effect(
        syllable(r, e, '', tone_blank), j, syllable(r, e, '', tone_dot)).

:- end_tests(apply_key_effect).

:- begin_tests(process_key).

test(append_to_empty_string, all(Key == [a, b, c, d, e, g, h, i, k, l, m, n, o, p, q, r, s, t, u, v, x, y, z])) :- 
    member(Key, [a, b, c, d, e, g, h, i, k, l, m, n, o, p, q, r, s, t, u, v, x, y, z]),
    process_key(Key, [], [Key]).

test(add_tone) :- process_key(s, [a], [á]).
test(add_mod_breve_a) :- process_key(w, [a], [ă]).
test(add_mod_d) :- process_key(d, [d], [đ]).

:- end_tests(process_key).

:- begin_tests(process_key_sequence).

test(append_to_empty_string) :- process_key_sequence([a], [a]).

test(simple_tone) :- 
    process_key_sequence([a, f], [à]),
    process_key_sequence([a, s], [á]),
    process_key_sequence([a, r], [ả]),
    process_key_sequence([a, x], [ã]),
    process_key_sequence([a, j], [ạ]).

test(fallback_to_raw_sequence) :-
    process_key_sequence([a, s, s], [a, s, s]).

:- end_tests(process_key_sequence).

:- begin_tests(process_atom).

test(quyeen) :- process_atom('quyeen', 'quyên').
test(quyeen) :- process_atom('dueenhf', 'duềnh').
test(bejche) :- process_atom('bejche', 'bệch').

:- end_tests(process_atom).

test :-
    findall((
        format("Test case: ~s ~s ~n", [A, B]),
        process_atom(A, B)
    ), test_case(A, B), Goals),
    maplist(call, Goals).