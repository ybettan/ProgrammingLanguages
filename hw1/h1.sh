#!/bin/bash

# Yonathan Bettan 302279138 yonibettan@gmail.com
# Alon Kwart 201025228 kwart@campus.technion.ac.il

wget -q -O - 'cs.technion.ac.il/courses/all/by-number/' | grep -E '<tr.*course_type_project.*><td>[0-9]{6}\s</td><td>.*Project.*</td><td>.*</td><td.*</td><td.*</td><td.*#EDEDED.*(href=\"http|img).*</td><td.*</td>' | sed -E 's/<tr class=.course_type_project.><td>//' | sed -E 's/<\/td><td>//' | sed -E 's/<\/td>.*<\/td>//' | sed -E 's/<\/tr>//'
