-module(lancaster1990).
-compile(export_all).

-record(stemRule, {
          wordEnd :: string(),
          intact  :: boolean(),
          removeTotal :: integer(),
          appendString :: string(),
          isContinuous  :: boolean()
         }).

-record(wordStatus, {
          value :: string(),
          modified = false :: boolean(),
          rules = [] :: list()                           
         }).

stemRule(Rule)->
    case re:run(Rule,"^([a-z]+)([\\*]*)([0-9]*)([a-z]*)([>\.]?)$",[{capture,all_but_first,list}]) of
        {match,[WordEnd, Intact, RemoveTotal, AppendString, IsContinuous]} ->
            Intact2 = case Intact of
                          []-> false;
                          "*" -> true
                      end,
            IsContinuous2 = case IsContinuous of
                          "."-> false;
                          ">" -> true
                      end,
            #stemRule{wordEnd = WordEnd,
                      intact = Intact2,
                      removeTotal = list_to_integer(RemoveTotal),
                      appendString = AppendString, 
                      isContinuous = IsContinuous2
                     };
        _ ->
            invalid
    end.

list_of_definitions_stemRule()->
    lists:map(fun(Rule)->
                      stemRule(Rule)
              end, list_of_definitions()).

%% Working
%% Denied
%% Proceed    
analyze_word(Word)->
    StemRules = list_of_definitions_stemRule(),
    %%io:format("StemRules:~n~p~n", [StemRules]),
    match_rule(string:to_lower(lists:reverse(Word)), #wordStatus{value = Word}, StemRules).

%% proceed
match_rule(_ReversedWord, WordStatus, []) ->
    io:format("WordStatus: ~p", [WordStatus]);
match_rule(ReversedWord, WordStatus, [StemRule | StemRules]) ->
    %% deecorp
    Ending = StemRule#stemRule.wordEnd,
    SubStr = string:substr(ReversedWord, 1,length(Ending)),
    case Ending == SubStr of
        true ->
            %%io:format("StemRule: ~p~n",[StemRule]),
            %%io:format("SubStr: ~p, Ending: ~p~n",[SubStr, Ending]),
            {NewReversedWord, NewWordStatus} = applay_the_rule(ReversedWord, WordStatus, StemRule),
            case StemRule#stemRule.isContinuous of
                true ->
                    match_rule(NewReversedWord, NewWordStatus, StemRules);
                false ->
                    match_rule(NewReversedWord, NewWordStatus, [])
            end;
         _ ->
            match_rule(ReversedWord, WordStatus, StemRules)
    end.
 
applay_the_rule(Word, WordStatus, StemRule) ->
    case StemRule#stemRule.intact of
        false -> 
            do_the_modifications(Word, WordStatus, StemRule);
        true ->
            case WordStatus#wordStatus.modified of
                true ->
                    {Word, WordStatus};
                false ->
                    do_the_modifications(Word, WordStatus, StemRule)
            end
    end.
            
do_the_modifications(Word, WordStatus, StemRule) ->
    Rem = StemRule#stemRule.removeTotal,
    Truncated = string:substr(Word, Rem+1),              %% deecorp, 3 => corp
    NewWord = StemRule#stemRule.appendString ++ Truncated,
    {NewWord, WordStatus#wordStatus{modified = true, 
                                    rules = [{StemRule,lists:reverse(Word),lists:reverse(NewWord)} |
                                             WordStatus#wordStatus.rules]}}.

%%------------------------------------------------------------------------------
%% A list of definitions of rules for English words which we want to modify 
%% and transform to basic form.
%%
%% Description how to read those rules:
%%  1. Inverted ending of the word (mandatory) - allows you to specify 
%%    whether the rule should be applied to the modified expression.
%%  2. Tag of inviolability (optional, symbol '*') - this means that the rule 
%%    should be applied only when the word is not affected by any previous rule.
%%  3. Number of characters removed (mandatory) - determines how many letters 
%%    at the end of a word should be removed during the modification.
%%  4. Substituted string (optional) - new end appended to the word after 
%%    the removal of the previous one.
%%  5. Flag of continuation (mandatory, symbol: '>' [true] or '.' [False]) - 
%%   specifies whether the application of this rule to the word limits any 
%%   further modifications
%%
%% Rules are taken from NLTK library (Natural Language Toolkit).
%%
%%------------------------------------------------------------------------------
list_of_definitions() ->
    ["ai*2.",     %% -ia > - if intact
     "a*1.",      %% -a > - if intact
     "bb1.",      %% -bb > -b
     "city3s.",   %% -ytic > -ys
     "ci2>",      %% -ic > -
     "cn1t>",     %% -nc > -nt
     "dd1.",      %% -dd > -d
     "dei3y>",    %% -ied > -y
     "deec2ss.",  %% -ceed >", -cess
     "dee1.",     %% -eed > -ee
     "de2>",      %% -ed > -
     "dooh4>",    %% -hood > -
     "e1>",       %% -e > -
     "feil1v.",   %% -lief > -liev
     "fi2>",      %% -if > -
     "gni3>",     %% -ing > -
     "gai3y.",    %% -iag > -y
     "ga2>",      %% -ag > -
     "gg1.",      %% -gg > -g
     "ht*2.",     %% -th > -   if intact
     "hsiug5ct.", %% -guish > -ct
     "hsi3>",     %% -ish > -
     "i*1.",      %% -i > -    if intact
     "i1y>",      %% -i > -y
     "ji1d.",     %% -ij > -id   --  see nois4j> & vis3j>
     "juf1s.",    %% -fuj > -fus
     "ju1d.",     %% -uj > -ud
     "jo1d.",     %% -oj > -od
     "jeh1r.",    %% -hej > -her
     "jrev1t.",   %% -verj > -vert
     "jsim2t.",   %% -misj > -mit
     "jn1d.",     %% -nj > -nd
     "j1s.",      %% -j > -s
     "lbaifi6.",  %% -ifiabl > -
     "lbai4y.",   %% -iabl > -y
     "lba3>",     %% -abl > -
     "lbi3.",     %% -ibl > -
     "lib2l>",    %% -bil > -bl
     "lc1.",      %% -cl > c
     "lufi4y.",   %% -iful > -y
     "luf3>",     %% -ful > -
     "lu2.",      %% -ul > -
     "lai3>",     %% -ial > -
     "lau3>",     %% -ual > -
     "la2>",      %% -al > -
     "ll1.",      %% -ll > -l
     "mui3.",     %% -ium > -
     "mu*2.",     %% -um > -   if intact
     "msi3>",     %% -ism > -
     "mm1.",      %% -mm > -m
     "nois4j>",   %% -sion > -j
     "noix4ct.",  %% -xion > -ct
     "noi3>",     %% -ion > -
     "nai3>",     %% -ian > -
     "na2>",      %% -an > -
     "nee0.",     %% protect  -een
     "ne2>",      %% -en > -
     "nn1.",      %% -nn > -n
     "pihs4>",    %% -ship > -
     "pp1.",      %% -pp > -p
     "re2>",      %% -er > -
     "rae0.",     %% protect  -ear
     "ra2.",      %% -ar > -
     "ro2>",      %% -or > -
     "ru2>",      %% -ur > -
     "rr1.",      %% -rr > -r
     "rt1>",      %% -tr > -t
     "rei3y>",    %% -ier > -y
     "sei3y>",    %% -ies > -y
     "sis2.",     %% -sis > -s
     "si2>",      %% -is > -
     "ssen4>",    %% -ness > -
     "ss0.",      %% protect  -ss
     "suo3>",     %% -ous > -
     "su*2.",     %% -us > -   if intact
     "s*1>",      %% -s > -    if intact
     "s0.",       %% -s > -s
     "tacilp4y.", %% -plicat > -ply
     "ta2>",      %% -at > -
     "tnem4>",    %% -ment > -
     "tne3>",     %% -ent > -
     "tna3>",     %% -ant > -
     "tpir2b.",   %% -ript > -rib
     "tpro2b.",   %% -orpt > -orb
     "tcud1.",    %% -duct > -duc
     "tpmus2.",   %% -sumpt > -sum
     "tpec2iv.",  %% -cept > -ceiv
     "tulo2v.",   %% -olut > -olv
     "tsis0.",    %% protect  -sist
     "tsi3>",     %% -ist > -
     "tt1.",      %% -tt > -t
     "uqi3.",     %% -iqu > -
     "ugo1.",     %% -ogu > -og
     "vis3j>",    %% -siv > -j
     "vie0.",     %% protect  -eiv
     "vi2>",      %% -iv > -
     "ylb1>",     %% -bly > -bl
     "yli3y>",    %% -ily > -y
     "ylp0.",     %% protect  -ply
     "yl2>",      %% -ly > -
     "ygo1.",     %% -ogy > -og
     "yhp1.",     %% -phy > -ph
     "ymo1.",     %% -omy > -om
     "ypo1.",     %% -opy > -op
     "yti3>",     %% -ity > -
     "yte3>",     %% -ety > -
     "ytl2.",     %% -lty > -l
     "yrtsi5.",   %% -istry > -
     "yra3>",     %% -ary > -
     "yro3>",     %% -ory > -
     "yfi3.",     %% -ify > -
     "ycn2t>",    %% -ncy > -nt
     "yca3>",     %% -acy > -
     "zi2>",      %% -iz > -
     "zy1s."].    %% -yz > -ys
