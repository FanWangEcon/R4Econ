# nested.list is a list of lists
# for nested.list, the first level names should not have dot
# dot should be what separates first and second level names after as.data.frame
# only list of list works

f_double_nested_list2df <- function(nested.list) {
  as.data.frame(nested.list) %>%
      gather(variable, value) %>%
      separate(variable, c('list.names', 'list.of.list.names'),
               sep = "\\.", extra = "merge") %>%
      spread(list.names, value) %>%
      column_to_rownames(var='list.of.list.names')
}


# Output:
# df_h0	df_h0wlt	df_i4xy
# convergence	0.0000000	0.0000000	0.0000000
# counts.function	20.0000000	108.0000000	55.0000000
# counts.gradient	NA	NA	NA
# par.frac.sum	1.0000000	1.0000000	1.0000000
# par.frac.v1	0.5109358	0.3599645	0.3740215
# par.frac.v2	0.4890642	0.2133391	0.3584842
# par.frac.v3	NA	0.1533452	0.2674943
# par.frac.v4	NA	0.2733512	NA
# par.v1	0.0437500	-0.5755182	-0.5150028
# par.v2	NA	-0.6931901	0.2927862
# par.v3	NA	-0.5780655	NA
# param.ces	1.0000000	1.0000000	1.0000000
# sca.subsidy.frac.init.v1	1.0000000	1.0000000	1.0000000
# sca.subsidy.frac.init.v2	NA	1.0000000	1.0000000
# sca.subsidy.frac.init.v3	NA	1.0000000	NA
# subsidy.total	100.0000000	100.0000000	100.0000000
# value	-84.7008162	-77.2168148	-93.2427961

# nested.list:
#
# $df_i4xy
#
#     $param.ces
#         1
#     $subsidy.total
#         100
#     $par.frac.sum
#         1
#     $value
#         -93.2427960670743
#     $counts.function
#         55
#     $counts.gradient
#         <NA>
#     $convergence
#         0
#     $sca.subsidy.frac.init.v1
#         1
#     $sca.subsidy.frac.init.v2
#         1
#     $par.v1
#         -0.515002762060613
#     $par.v2
#         0.292786154174249
#     $par.frac.v1
#         0.374021495553532
#     $par.frac.v2
#         0.35848417578872
#     $par.frac.v3
#         0.267494328657749
#
# $df_h0
#
#     $param.ces
#         1
#     $subsidy.total
#         100
#     $par.frac.sum
#         1
#     $value
#         -84.7008162301391
#     $counts.function
#         20
#     $counts.gradient
#         <NA>
#     $convergence
#         0
#     $sca.subsidy.frac.init.v1
#         1
#     $par.v1
#         0.0437499999999992
#     $par.frac.v1
#         0.510935755745644
#     $par.frac.v2
#         0.489064244254356
#
# $df_h0wlt
#
#     $param.ces
#         1
#     $subsidy.total
#         100
#     $par.frac.sum
#         1
#     $value
#         -77.2168148010811
#     $counts.function
#         108
#     $counts.gradient
#         <NA>
#     $convergence
#         0
#     $sca.subsidy.frac.init.v1
#         1
#     $sca.subsidy.frac.init.v2
#         1
#     $sca.subsidy.frac.init.v3
#         1
#     $par.v1
#         -0.575518159412908
#     $par.v2
#         -0.693190108288177
#     $par.v3
#         -0.57806553766017
#     $par.frac.v1
#         0.359964515822228
#     $par.frac.v2
#         0.213339055820869
#     $par.frac.v3
#         0.153345238534249
#     $par.frac.v4
#         0.273351189822654
#
