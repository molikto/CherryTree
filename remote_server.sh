git stash
git pull
sbt assembly
cd jvm
java -jar target/scala-2.12/server-assembly-0.1.0-SNAPSHOT.jar