sudo lsof -i tcp:443 | sudo awk 'NR!=1 {print $2}' | sudo xargs kill
sudo rm -rf server
unzip server.zip
mv server-0.1.0-SNAPSHOT server
cd server
chmod a+x bin/server
sudo bin/server -Dconfig.resource=application.prod.conf -Dhttp.port=disabled -Dhttps.port=443 -Dplay.server.https.keyStore.path="/home/ec2-user/cherrytree.keystore" -Dplay.server.https.keyStore.password=fksde2#aj_i3_lkj32jkfdsali34j2kl -Dplay.crypto.secret=jkf,d,djs!ifj#al2314hkdji -Dplay.mailer.user=AKIAJ3T3UXVWPZR2ZTRQ -Dplay.mailer.password=AptDE8qTnZtTwWjZYvijeGOwTVbRk0B90JqIbO7DkgUo -Dslick.dbs.default.db.url="jdbc:postgresql://cherrytree.cxm8uuo2kg08.us-west-2.rds.amazonaws.com:5432/cherrytree" -Dslick.dbs.default.db.user=molikto -Dslick.dbs.default.db.password=molikto-cherrytree -Dgoogle.clientID="488690993377-qtllhle3frdtj781eo340554bgbktacj.apps.googleusercontent.com" -Dgoogle.clientSecret="IpGnmC9Ay5sbxG3L5B3sk-3T"
# when generating keystore
# sudo bin/server  -Dconfig.resource=application.prod.conf -Dhttp.port=80 -Dhttps.port=443 -Dplay.server.https.keyStore.path=/home/ec2-user/cherrytree.keystore -Dplay.user.keyStore.path=conf/cherrytree.pvt.keystore -Dplay.server.https.keyStore.password=fksde2#aj_i3_lkj32jkfdsali34j2kl -Djdk.tls.ephemeralDHKeySize=2048 -Dplay.evolutions.db.default.autoApply=true -Dplay.crypto.secret=jkf,d,djs!ifj#al2314hkdji
