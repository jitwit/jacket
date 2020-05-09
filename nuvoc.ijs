require'regex web/gethttp'
coinsert'jregex'

jesq=: '/Users/jrn/code/jdoc' 
NuvocURL=: 'https://code.jsoftware.com/wiki/NuVoc'
NuvocHtml=: 'data/Nuvoc'

NB. topics seem to start with capitals. this should find the
NB. interesting links
vocpat=: '/wiki/Vocabulary/[[:lower:]|#]+'
verpat=: '<tt>.{1,9}(?=</tt>)'

FetchNuvoc=: 3 : 0
assert. jesq -: 1!:43''
if. -. fexist NuvocHtml do. ('-o ',NuvocHtml) gethttp NuvocURL end.
)

FetchNuvocDoc=: 3 : 0
assert. jesq -: 1!:43''
if. -. fexist y do. ('-o ',y) gethttp y end.
)

Nuvoc=: 1!:1 < NuvocHtml
Links=: vocpat rxall Nuvoc


