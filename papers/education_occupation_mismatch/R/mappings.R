# Factual coding correspondence: Montt (2015), Annex 2, SAS specification pp.50–51.
# Use the executable specification: its health-field mapping differs from the prose.
field_matches<-list(
 `2`=c(231:235,342,531),
 `3`=c(216,231:233,262:265,341,343),
 `4`=c(112,121:122,131:134,141:143,226,231:233,241:243,261:264,325,331:335,341,411:413,421:422,431:432,441,521:524,952),
 `5`=c(211:213,226,231:233,251:252,311,313:314,321,331,351:352),
 `6`=c(214:216,231:232,251:252,311:313,315,335,351:352,515,711:713,721:723,731:732,741:742,751:754,811:818,821,831:835,931:933),
 `7`=c(213,225,231:232,314,321,324:325,611:613,621:622,631:634,751,754,834,921),
 `8`=c(213,221:224,226:227,231,234,263,314,321:325,341,516,531:532,541),
 `9`=c(134,243,325,334:335,341,343,411:413,421:422,431:432,441,511:516,521:524,531:532,541,831:835,911:912,941,951:952))
valid_minor<-sort(unique(c(unlist(field_matches),111,961,962)))
field9<-function(x) {
 map<-c(4,4,8,8,4,4,5,5,6,6,7,9,9,2,3,1)
 ifelse(x %in% 1:16,map[pmax(1,pmin(16,x))],NA_real_)
}
parse_occ<-function(full,two,one) {
 # Suppression codes 9999/9996 and short aggregate values are never occupations.
 x<-trimws(as.character(full)); n<-suppressWarnings(as.numeric(x))
 minor<-ifelse(grepl('^[1-9][0-9]{3}$',x),floor(n/10),
  ifelse(grepl('^[1-9][0-9]{2}$',x),n,NA))
 minor[!minor %in% valid_minor]<-NA
 sub<-suppressWarnings(as.numeric(as.character(two)))
 valid_sub<-c(11:14,21:26,31:35,41:44,51:54,61:63,71:75,81:83,91:96)
 sub[!sub %in% valid_sub]<-NA
 sub[!is.na(minor)]<-floor(minor[!is.na(minor)]/10)
 major<-suppressWarnings(as.numeric(as.character(one)));major[!major %in% 1:9]<-NA
 major[!is.na(sub)]<-floor(sub[!is.na(sub)]/10)
 data.frame(minor=minor,major=major)
}
ilo_vertical<-function(major,level,grouped) {
 # User-specified ISCED-2011 matrix. Managers 5–8, professionals 6–8,
 # technicians 5, groups 4–8: 2–4, elementary: 1.
 lo<-ifelse(level %in% 0:8,level,NA_real_);hi<-lo
 ix<-is.na(lo) & grouped %in% 1:7
 lo[ix]<-ifelse(grouped[ix]==1,0,grouped[ix]);hi[ix]<-ifelse(grouped[ix]==7,8,grouped[ix])
 required_lo<-c(5,6,5,2,2,2,2,2,1)[major]
 required_hi<-c(8,8,5,4,4,4,4,4,1)[major]
 certain<-function(a,b)ifelse(!is.na(a)&!is.na(b)&a==b,as.numeric(a),NA_real_)
 over<-certain(lo>required_hi,hi>required_hi)
 under<-certain(lo<required_lo,hi<required_lo)
 matched<-certain(lo>=required_lo & lo<=required_hi,hi>=required_lo & hi<=required_hi)
 data.frame(vertical=over,undereducated=under,vertical_match=matched,
  education_level=ifelse(is.na(lo),NA,pmax(1,pmin(7,hi))),education_aggregated=ix)
}
horizontal_code<-function(field,minor) {
 out<-rep(NA_real_,length(field))
 ok<-field %in% 2:9 & minor %in% setdiff(valid_minor,c(111,961,962))
 for(f in 2:9) {ix<-ok & field==f;out[ix]<-as.numeric(!minor[ix] %in% field_matches[[as.character(f)]])}
 out
}
