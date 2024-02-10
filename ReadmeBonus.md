# COP5615_Chord Protocol_Project_Bonus

## Team Members

- Rishabh Srivastav : UFID 7659-9488
- Ashish Sunny Abraham : UFID 6388-7782

## Outline of the project
- This project aims to design the Chord protocol and a simplistic object access service to demonstrate the Chord protocol's utility using Erlang and Actor Model.

## Commands to run the program
- Compile all the .erl files in the zip folder (main, nodeCreation, stabilizeChordRing, chordRing, utilityFunctions). 
- Run the project by running the command “main:start().” in the erl shell. 
- Enter the number of nodes, message requests and failure nodes, for example, 1000,10,10.


## What is working?
- Upon incorporating the failure nodes strategy in the existing chord protocol, it was found that the average number of hops required for a node to search another node seemed to increase compared to when there were no failure nodes.
- We observed that as the number of failure nodes increases, the average number of hops also increases.
- During our observation of the implementation of failure nodes, we tried to gather results for 10 failure nodes with 10 messages each. This time, the average number of hops came out to be 3.95 when it was averaged across 20 runs which is an increase over the previous implementation of no failure nodes.

## Observations
| Number of Nodes | Average Hops |
| --------------- |:------------:|
| 100             | 2.9          |
| 500             | 3.6          |
| 1000            | 4.1          |
| 2000            | 5.2          |

**Running program for 1000 nodes, 10 message requests, 10 failure nodes:**
<img width="849" alt="Screen Shot 2022-10-23 at 6 33 19 PM" src="https://user-images.githubusercontent.com/59756917/197421432-557070f9-e21d-4c1f-bd76-5cdca55bda93.png">

