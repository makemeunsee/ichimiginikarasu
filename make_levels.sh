for i in 10 9 8 7 6 5 4 3 pre2 2
do
  time stack exec -- ichimiginikarasu-exe -j resources/JMdict -f resources/jpn_words_girardi_kelly -i level_${i} -n "${i}級" > level_${i}.tex
  time stack exec -- ichimiginikarasu-exe -j resources/JMdict -f resources/jpn_words_girardi_kelly -i level_${i} -n "${i}級" -l fr > level_${i}_fr.tex
done
