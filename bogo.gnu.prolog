% GNU Prolog doesn't support Unicode so we're just doing calculation directly on streams of UTF-8
% char codes here. Note that the letters do not have same byte lengths. For example, "a" takes 1 byte
% but "ả" takes 3. The double quote syntax stores a string as a list of bytes.

vi_lower_upper_vowel("à", "À").
vi_lower_upper_vowel("á", "Á").
vi_lower_upper_vowel("ả", "Ả").
vi_lower_upper_vowel("ã", "Ã").
vi_lower_upper_vowel("ạ", "Ạ").
vi_lower_upper_vowel("a", "A").

vi_lower_upper_vowel("ằ", "Ằ").
vi_lower_upper_vowel("ắ", "Ắ").
vi_lower_upper_vowel("ẳ", "Ẳ").
vi_lower_upper_vowel("ẵ", "Ẵ").
vi_lower_upper_vowel("ặ", "Ặ").
vi_lower_upper_vowel("ă", "Ă").

vi_lower_upper_vowel("ầ", "Ầ").
vi_lower_upper_vowel("ấ", "Ấ").
vi_lower_upper_vowel("ẩ", "Ẩ").
vi_lower_upper_vowel("ẫ", "Ẫ").
vi_lower_upper_vowel("ậ", "Ậ").
vi_lower_upper_vowel("â", "Â").

vi_lower_upper_vowel("è", "È").
vi_lower_upper_vowel("é", "É").
vi_lower_upper_vowel("ẻ", "Ẻ").
vi_lower_upper_vowel("ẽ", "Ẽ").
vi_lower_upper_vowel("ẹ", "Ẹ").
vi_lower_upper_vowel("e", "E").

vi_lower_upper_vowel("ề", "Ề").
vi_lower_upper_vowel("ế", "Ế").
vi_lower_upper_vowel("ể", "Ể").
vi_lower_upper_vowel("ễ", "Ễ").
vi_lower_upper_vowel("ệ", "Ệ").
vi_lower_upper_vowel("ê", "Ê").

vi_lower_upper_vowel("ì", "Ì").
vi_lower_upper_vowel("í", "Í").
vi_lower_upper_vowel("ỉ", "Ỉ").
vi_lower_upper_vowel("ĩ", "Ĩ").
vi_lower_upper_vowel("ị", "Ị").
vi_lower_upper_vowel("i", "I").

vi_lower_upper_vowel("ò", "Ò").
vi_lower_upper_vowel("ó", "Ó").
vi_lower_upper_vowel("ỏ", "Ỏ").
vi_lower_upper_vowel("õ", "Õ").
vi_lower_upper_vowel("ọ", "Ọ").
vi_lower_upper_vowel("o", "O").

vi_lower_upper_vowel("ồ", "Ồ").
vi_lower_upper_vowel("ố", "Ố").
vi_lower_upper_vowel("ổ", "Ổ").
vi_lower_upper_vowel("ỗ", "Ỗ").
vi_lower_upper_vowel("ộ", "Ộ").
vi_lower_upper_vowel("ô", "Ô").

vi_lower_upper_vowel("ờ", "Ờ").
vi_lower_upper_vowel("ớ", "Ớ").
vi_lower_upper_vowel("ở", "Ở").
vi_lower_upper_vowel("ỡ", "Ỡ").
vi_lower_upper_vowel("ợ", "Ợ").
vi_lower_upper_vowel("ơ", "Ơ").

vi_lower_upper_vowel("ù", "Ù").
vi_lower_upper_vowel("ú", "Ú").
vi_lower_upper_vowel("ủ", "Ủ").
vi_lower_upper_vowel("ũ", "Ũ").
vi_lower_upper_vowel("ụ", "Ụ").
vi_lower_upper_vowel("u", "U").

vi_lower_upper_vowel("ừ", "Ừ").
vi_lower_upper_vowel("ứ", "Ứ").
vi_lower_upper_vowel("ử", "Ử").
vi_lower_upper_vowel("ữ", "Ữ").
vi_lower_upper_vowel("ự", "Ự").
vi_lower_upper_vowel("ư", "Ư").

vi_lower_upper_vowel("ỳ", "Ỳ").
vi_lower_upper_vowel("ý", "Ý").
vi_lower_upper_vowel("ỷ", "Ỷ").
vi_lower_upper_vowel("ỹ", "Ỹ").
vi_lower_upper_vowel("ỵ", "Ỵ").
vi_lower_upper_vowel("y", "Y").

vi_lower_upper("đ", "Đ").

vi_lower_upper(L, U) :- vi_lower_upper_vowel(L, U).

vi_lower_upper([L], [U]) :-
    between(97, 122, L),
    between(65, 89, U),
    char_code(LC, L),
    lower_upper(LC, UC),
    char_code(UC, U).

vi_lower_upper([C], [C]) :-
    between(0, 127, C),
    \+ between(97, 122, C),
    \+ between(65, 98, C).

case(C, lower) :- vi_lower_upper(C, _).
case(C, upper) :- vi_lower_upper(_, C).

letters([L|Rest], [Case|CaseRest]) --> letter(L, Case), letters(Rest, CaseRest).
letters([], []) --> [].

letter(C, lower, Input, Rest) :-
    vi_lower_upper(C, _),
    append(C, Rest, Input).

letter(CLower, upper, Input, Rest) :-
    vi_lower_upper(CLower, C),
    append(C, Rest, Input).

string_letters(String, Letters, Cases) :-
    phrase(letters(Letters, Cases), String).

% TELEX
key_effect("f", add_tone(tone_huyen)).
key_effect("s", add_tone(tone_sac)).
key_effect("r", add_tone(tone_hoi)).
key_effect("x", add_tone(tone_nga)).
key_effect("j", add_tone(tone_nang)).

key_effect("a", add_vowel_mod(mod_hat_a)).
key_effect("e", add_vowel_mod(mod_hat_e)).
key_effect("o", add_vowel_mod(mod_hat_o)).
key_effect("w", add_vowel_mod(mod_horn_uo)).
key_effect("w", add_vowel_mod(mod_horn_o)).
key_effect("w", add_vowel_mod(mod_horn_u)).
key_effect("w", add_vowel_mod(mod_breve_a)).

key_effect("d", add_consonant_mod(mod_dash_d)).

% VNI
key_effect("1", add_tone(tone_sac)).
key_effect("2", add_tone(tone_huyen)).
key_effect("3", add_tone(tone_hoi)).
key_effect("4", add_tone(tone_nga)).
key_effect("5", add_tone(tone_nang)).

key_effect("6", add_vowel_mod(mod_hat_a)).
key_effect("6", add_vowel_mod(mod_hat_e)).
key_effect("6", add_vowel_mod(mod_hat_o)).
key_effect("7", add_vowel_mod(mod_horn_uo)).
key_effect("7", add_vowel_mod(mod_horn_o)).
key_effect("7", add_vowel_mod(mod_horn_u)).
key_effect("8", add_vowel_mod(mod_breve_a)).

key_effect("9", add_consonant_mod(mod_dash_d)).


process_key_sequence(Sequence, Output) :-
    process_key_sequence(Sequence, "", Output).

process_key_sequence([Key|Rest], CurrentString, Output) :-
    process_key(CurrentString, [Key], Output1),
    process_key_sequence(Rest, Output1, Output).

process_key_sequence([], CurrentString, CurrentString).

% fall back to the raw sequence if processing fails (vowel -> vơel)
process_key_sequence(Keys, [], Keys).


% Try applying the effect of the key, assuming that there is one.
% Performance optimization idea: give out both an output string and a syllable term and reuse the term for the next cycle.
process_key(CurrentString, Key, OutString) :-
    % save the case information
    string_letters(CurrentString, InLetters, InCases),
    flatten(InLetters, CurrentStringLower),

    letter(KeyLower, KeyCase, Key, []),

    % break the syllable down into initial consonant, vowel nucleus and final consonant
    phrase(syllable(S), CurrentStringLower),

    apply_key_effect(S, KeyLower, Out),
    rebalance_incomplete_form(Out, s(I, V, F)),
    flatten([I, V, F], OutStringLower),

    % require that the output must be parsable, this is to weed out non-Vietnamese words like vowel -> vơel
    phrase(syllable(_), OutStringLower),

    string_letters(OutStringLower, OutLetters, _),

    (
        (length(InLetters, N), length(OutLetters, N)) 
    ->
        string_letters(OutString, OutLetters, InCases)
    ;
        append(InCases, [KeyCase], FinalCases),
        string_letters(OutString, OutLetters, FinalCases)
    ).

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_tone(Tone)),
    syllable_tone(InSyllable, Tone, OutSyllable), !.

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_vowel_mod(Mod)),
    syllable_vowel_mod(InSyllable, Mod, OutSyllable), !.

apply_key_effect(InSyllable, Key, OutSyllable) :-
    key_effect(Key, add_consonant_mod(Mod)),
    syllable_consonant_mod(InSyllable, Mod, OutSyllable), !.

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
syllable_consonant_mod(s("d", V, F), _, s("đ", V, F)).

syllable(s(I, V, F)) --> consonant_initial(I), vowel_chars(V), consonant_final(F).

vowel_chars(Result) --> vowel_char(X), vowel_chars(Rest), { append(X, Rest, Result) }.
vowel_chars([]) --> [].

vowel_char(C, Input, Rest) :-
    vi_lower_upper_vowel(C, _),
    append(C, Rest, Input).

consonant_initial("") --> [].
consonant_initial("m") --> "m".
consonant_initial("n") --> "n".
consonant_initial("nh") --> "nh".
consonant_initial("ng") --> "ng".
consonant_initial("ngh") --> "ngh".
consonant_initial("p") --> "p".
consonant_initial("t") --> "t".
consonant_initial("tr") --> "tr".
consonant_initial("ch") --> "ch".
consonant_initial("c") --> "c".
consonant_initial("k") --> "k".
consonant_initial("qu") --> "qu".
consonant_initial("q") --> "q". % I'm a bit iffy on this case since it allows typing teencode-looking words like qán, qẩn
consonant_initial("ph") --> "ph".
consonant_initial("th") --> "th".
consonant_initial("kh") --> "kh".
consonant_initial("b") --> "b".
consonant_initial("đ") --> "đ".
consonant_initial("s") --> "s".
consonant_initial("x") --> "x".
consonant_initial("h") --> "h".
consonant_initial("d") --> "d".
% già
consonant_initial("gi"), [V] --> "gi", vowel_chars(V).
% gì, gà
consonant_initial("g") --> "g".
consonant_initial("gh") --> "gh".
consonant_initial("v") --> "v".
consonant_initial("l") --> "l".
consonant_initial("r") --> "r".
% for the dzũng zũng people
consonant_initial("dz") --> "dz".
consonant_initial("z") --> "z".

consonant_final("") --> [].
consonant_final("n") --> "n".
consonant_final("nh") --> "nh".
consonant_final("ng") --> "ng".
consonant_final("c") --> "c".
consonant_final("ch") --> "ch".
consonant_final("p") --> "p".
consonant_final("t") --> "t".
consonant_final("n") --> "n".
consonant_final("m") --> "m".



% vowel nucleus transformation table schema:
%   resulting syllable, base syllable, vowel mod, tone, allow incomplete
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_ngang) :- terminal_vowel(Raw, Mod, (Result, _, _, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_huyen) :- terminal_vowel(Raw, Mod, (_, Result, _, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_sac)   :- terminal_vowel(Raw, Mod, (_, _, Result, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_hoi)   :- terminal_vowel(Raw, Mod, (_, _, _, Result, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_nga)   :- terminal_vowel(Raw, Mod, (_, _, _, _, Result, _)), !.
vowel_nucleus_mod_tone(s(I, Result, ""), s(I, Raw, ""), Mod, tone_nang)  :- terminal_vowel(Raw, Mod, (_, _, _, _, _, Result)), !.

vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_ngang) :- not_strictly_terminal_vowel(Raw, Mod, (Result, _, _, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_huyen) :- not_strictly_terminal_vowel(Raw, Mod, (_, Result, _, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_sac)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, Result, _, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_hoi)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, Result, _, _)), !.
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_nga)   :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, _, Result, _)), !.
vowel_nucleus_mod_tone(s(I, Result, F), s(I, Raw, F), Mod, tone_nang)  :- not_strictly_terminal_vowel(Raw, Mod, (_, _, _, _, _, Result)), !.

vowel_nucleus_mod_tone(s(I, V, F), s(I, V, F), mod_none, tone_ngang).


% not_strictly_terminal_vowel means the vowel can be both terminal and non-terminal.
% Would it be useful to also distinguish strictly non-terminal vowels?
not_strictly_terminal_vowel("a", mod_breve_a, ("ă", "ằ", "ắ", "ẳ", "ẵ", "ặ")).
not_strictly_terminal_vowel("a", mod_hat_a, ("â", "ầ", "ấ", "ẩ", "ẫ", "ậ")).
not_strictly_terminal_vowel("a", mod_none, ("a", "à", "á", "ả", "ã", "ạ")).
not_strictly_terminal_vowel("e", mod_hat_e, ("ê", "ề", "ế", "ể", "ễ", "ệ")).
not_strictly_terminal_vowel("e", mod_none, ("e", "è", "é", "ẻ", "ẽ", "ẹ")).
not_strictly_terminal_vowel("i", mod_none, ("i", "ì", "í", "ỉ", "ĩ", "ị")).
not_strictly_terminal_vowel("ie", mod_hat_e, ("iê", "iề", "iế", "iể", "iễ", "iệ")).
not_strictly_terminal_vowel("o", mod_hat_o, ("ô", "ồ", "ố", "ổ", "ỗ", "ộ")).
not_strictly_terminal_vowel("o", mod_horn_o, ("ơ", "ờ", "ớ", "ở", "ỡ", "ợ")).
not_strictly_terminal_vowel("o", mod_none, ("o", "ò", "ó", "ỏ", "õ", "ọ")).
not_strictly_terminal_vowel("oa", mod_breve_a, ("oă", "oằ", "oắ", "oẳ", "oẵ", "oặ")).
not_strictly_terminal_vowel("oa", mod_none, ("oa", "oà", "oá", "oả", "oã", "oạ")).
not_strictly_terminal_vowel("u", mod_horn_u, ("ư", "ừ", "ứ", "ử", "ữ", "ự")).
not_strictly_terminal_vowel("u", mod_none, ("u", "ù", "ú", "ủ", "ũ", "ụ")).
not_strictly_terminal_vowel("ue", mod_hat_e, ("uê", "uề", "uế", "uể", "uễ", "uệ")).
not_strictly_terminal_vowel("ua", mod_hat_a, ("uâ", "uầ", "uấ", "uẩ", "uẫ", "uậ")).
not_strictly_terminal_vowel("uo", mod_hat_o, ("uô", "uồ", "uố", "uổ", "uỗ", "uộ")).
not_strictly_terminal_vowel("uo", mod_horn_uo, ("ươ", "ườ", "ướ", "ưở", "ưỡ", "ượ")).
not_strictly_terminal_vowel("uo", mod_horn_uo, ("uơ", "uờ", "uớ", "uở", "uỡ", "uợ")).
% huyền, thuyền
not_strictly_terminal_vowel(uye, mod_hat_e, ("uyê", "uyề", "uyế", "uyể", "uyễ", "uyệ")).
% quyền and yến
not_strictly_terminal_vowel(ye, mod_hat_e, ("yê", "yề", "yế", "yể", "yễ", "yệ")).

% test :-
%     vowel_nucleus_mod_tone(s(I, V, F), s("m", "a", "m"), mod_none, tone_huyen),
%     format("~s~s~s", [I, V, F]).


% The incomplete forms happen as transitional states before the final word. For example, if you type huyenf then you get
% huyèn, an arguably invalid VNmese word, but if you type 'e' after that, you get huyền, a valid word.
% We don't do anything with the incomplete forms yet but it's a good distintion to make, at least at the mental level.
not_strictly_terminal_vowel(A, B, C) :- not_strictly_terminal_vowel_incomplete(A, B, C).

not_strictly_terminal_vowel_incomplete("uo", mod_none, ("uo", "uò", "uó", "uỏ", "uõ", "uọ")).
not_strictly_terminal_vowel_incomplete("uo", mod_horn_uo, ("ưo", "ừo", "ứo", "ửo", "ữo", "ựo")).
not_strictly_terminal_vowel_incomplete("uo", mod_horn_uo, ("ưo", "ưò", "ưó", "ưỏ", "ưõ", "ưọ")).
not_strictly_terminal_vowel_incomplete("uye", mod_none, ("uye", "uyè", "uyé", "uyẻ", "uyẽ", "uyẹ")).
not_strictly_terminal_vowel_incomplete("ie", mod_none, ("ie", "iè", "ié", "iẻ", "iẽ", "iẹ")).
not_strictly_terminal_vowel_incomplete("ie", mod_none, ("ie", "ìe", "íe", "ỉe", "ĩe", "ịe")).
not_strictly_terminal_vowel_incomplete("ue", mod_none, ("ue", "uè", "ué", "uẻ", "uẽ", "uẹ")).

terminal_vowel("ai", mod_none, ("ai", "ài", "ái", "ải", "ãi", "ại")).
terminal_vowel("ao", mod_none, ("ao", "ào", "áo", "ảo", "ão", "ạo")).
terminal_vowel("au", mod_hat_a, ("âu", "ầu", "ấu", "ẩu", "ẫu", "ậu")).
terminal_vowel("au", mod_none, ("au", "àu", "áu", "ảu", "ãu", "ạu")).
terminal_vowel("ay", mod_hat_a, ("ây", "ầy", "ấy", "ẩy", "ẫy", "ậy")).
terminal_vowel("ay", mod_none, ("ay", "ày", "áy", "ảy", "ãy", "ạy")).
terminal_vowel("eo", mod_none, ("eo", "èo", "éo", "ẻo", "ẽo", "ẹo")).
terminal_vowel("eu", mod_hat_e, ("êu", "ều", "ếu", "ểu", "ễu", "ệu")).
terminal_vowel("ia", mod_none, ("ia", "ìa", "ía", "ỉa", "ĩa", "ịa")).
terminal_vowel("ie", mod_none, ("ie", "ìe", "íe", "ỉe", "ĩe", "ịe")).
terminal_vowel("ieu", mod_hat_e, ("iêu", "iều", "iếu", "iểu", "iễu", "iệu")).
terminal_vowel("iu", mod_none, ("iu", "ìu", "íu", "ỉu", "ĩu", "ịu")).
terminal_vowel("oa", mod_none, ("oa", "oà", "oá", "oả", "oã", "oạ")) :- get_flag(bogo:dau_moi, true).
terminal_vowel("oa", mod_none, ("oa", "òa", "óa", "ỏa", "õa", "ọa")).
terminal_vowel("oe", mod_none, ("oe", "oè", "oé", "oẻ", "oẽ", "oẹ")) :- get_flag(bogo:dau_moi, true).
terminal_vowel("oe", mod_none, ("oe", "òe", "óe", "ỏe", "õe", "ọe")).
terminal_vowel("oi", mod_none, ("oi", "òi", "ói", "ỏi", "õi", "ọi")).
terminal_vowel("ua", mod_none, ("ua", "ùa", "úa", "ủa", "ũa", "ụa")).
terminal_vowel("ui", mod_horn_u, ("ưi", "ừi", "ứi", "ửi", "ữi", "ựi")).
terminal_vowel("ui", mod_none, ("ui", "ùi", "úi", "ủi", "ũi", "ụi")).
terminal_vowel("uo", mod_horn_uo, ("uơ", "uờ", "uớ", "uở", "uỡ", "uợ")).
terminal_vowel("uoi", mod_hat_o, ("uôi", "uồi", "uối", "uổi", "uỗi", "uội")).
terminal_vowel("uoi", mod_horn_uo, ("ươi", "ười", "ưới", "ưởi", "ưỡi", "ượi")).
terminal_vowel("uou", mod_horn_uo, ("ươu", "ườu", "ướu", "ưởu", "ưỡu", "ượu")).
terminal_vowel("uu", mod_horn_u, ("ưu", "ừu", "ứu", "ửu", "ữu", "ựu")).
terminal_vowel("uy", mod_none, ("uy", "uỳ", "uý", "uỷ", "uỹ", "uỵ")) :- get_flag(bogo:dau_moi, true).
terminal_vowel("uy", mod_none, ("uy", "ùy", "úy", "ủy", "ũy", "ụy")).
terminal_vowel("y", mod_none, ("y", "ỳ", "ý", "ỷ", "ỹ", "ỵ")).
terminal_vowel("ya", mod_none, ("ya", "ỳa", "ýa", "ỷa", "ỹa", "ỵa")).

terminal_vowel(A, B, C) :- terminal_vowel_incomplete(A, B, C).

terminal_vowel_incomplete("uoi", mod_none, ("uoi", "uòi", "uói", "uỏi", "uõi", "uọi")).
terminal_vowel_incomplete("uoi", mod_none, ("uoi", "uòi", "uói", "uỏi", "uõi", "uọi")).
terminal_vowel_incomplete("uoi", mod_horn_uo, ("uơi", "uời", "uới", "uởi", "uỡi", "uợi")).
terminal_vowel_incomplete("uou", mod_none, ("uou", "uòu", "uóu", "uỏu", "uõu", "uọu")).