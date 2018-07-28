rm -rf jvm/saved
rm -rf jvm/changed
git stash
git pull
sbt assembly
cd jvm
PORT_NUMBER=8080
lsof -i tcp:${PORT_NUMBER} | awk 'NR!=1 {print $2}' | xargs kill
java -jar target/scala-2.12/server-assembly-0.1.0-SNAPSHOT.jar &