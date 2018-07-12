numberOfCores=3
for filename in ./bin/*; do
    echo "Tests for $filename"
    $filename +RTS -N$numberOfCores
    echo
done
echo "Done!"
