grep -v Escuelas ../raw_data/denue_inegi_31_.csv | awk -vFPAT='([^,]*)|("[^"]+")' -vOFS=, '{print $6,$39,$40}' | tr -d '"' > ../derived_data/non_school_denue_data.out
