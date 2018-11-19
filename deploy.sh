#!/usr/bin/env bash
sbt universal:dist
scp -i "../aws/molikto.pem" jvm/target/universal/server-0.1.0-SNAPSHOT.zip ec2-user@ec2-54-203-4-11.us-west-2.compute.amazonaws.com:~/server.zip
ssh -i "../aws/molikto.pem" ec2-user@ec2-54-203-4-11.us-west-2.compute.amazonaws.com < production.sh

