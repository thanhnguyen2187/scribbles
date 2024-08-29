#!/bin/sh

CHAPTER="$1"
NUMBER="$2"

cp \
    ./src/content/templates/sdf-exercise.mdx \
    ./src/content/docs/sdf/chapter-$CHAPTER/exercise-$CHAPTER-$NUMBER.mdx \
;

cp \
    code/sdf/exercise-answers/template.scm \
    code/sdf/exercise-answers/$CHAPTER-$NUMBER.scm \
;
