# CSCS-USI HPC Summer School 2019

This repository contains the materials used in the Summer School, including source code, lecture notes and slides.
Material will be added to the repository throughout the course, which will require that students either update their copy of the repository, or download/checkout a new copy of the repository.

## Announcements

- Instructions to prepare the deep learning environment (before spawning your jupyter server):
```
source /users/stud31/startup.sh
```
- [Sunday Hiking Map](https://raw.githubusercontent.com/eth-cscs/SummerSchool2019/master/Sunday%20Hiking%20Directions.png): Departure 10:30 bus stop in front of Steger Center.
- [Thursday 18th 18:00-19:00]: Guided walking tour of Riva San Vitale by Daniela. Meeting point: Lobby in front of Daniela's office.
- [Schedule and logistics for Tuesday July 16th](https://github.com/eth-cscs/SummerSchool2019/blob/master/Tuesday%2016th%20schedule.pdf): Travel to Lugano, CSCS visit and Evening event.

## Schedule

![Schedule](Updated-Schedule.png "Summer School Schedule")

## Getting the repository

### On your own computer

You will want to download the repository to your laptop to get all of the slides.
The best method is to use git, so that you can update the repository as more slides and material are added over the course of the school.
So, if you have git installed, use the same method as for Piz Daint below (in a directory of your choosing).

You can also download the code as a zip file by clicking on the green __Clone or download__ button on the top right hand side of the github page, then clicking on __Download zip__.

### On Piz Daint

Piz Daint has git installed, so we can download the code directly to where we will be working on the exercises and practicals.

```bash
# log onto Piz Daint ...

# go to scratch
cd $SCRATCH

# it is a good policy to put it in a personal path
mkdir johnsmith
cd johnsmith
git clone https://github.com/eth-cscs/SummerSchool2019.git
```

## Updating the repository

Lecture slides and source code will be continually added and updated throughout the course.
To update the repository you can simply go inside the path

```
git pull origin master
```

There is a posibility that you might have a conflict between your working version of the repository and the origin.
In this case you can ask one of the assistants for help.
