# COP5615_Chord Protocol_Project

## Team Members

- Rishabh Srivastav : UFID 7659-9488
- Ashish Sunny Abraham : UFID 6388-7782

## Outline of the project
- This project aims to design the Chord protocol and a simplistic object access service to demonstrate the Chord protocol's utility using Erlang and Actor Model.

## Commands to run the program
- Compile all the .erl files in the zip folder (main, nodeCreation, stabilizeChordRing, chordRing, utilityFunctions). 
- Run the project by running the command “main:start().” in the erl shell. 
- Enter the number of nodes, message requests and failure nodes, for example, 1000,10,0 (Refer to bonus report for failure node implementation).

## Implementation Details

**Chord Protocol:**
- In our implementation of the chord protocol, we ask the user to enter the number of nodes ‘n’, based on which we create 2^m nodes (M) where m = ceiling of log2n.
- We then select n nodes from 1 to M to participate in the algorithm. The remaining M-n nodes are missing and their information will be stored with their successor.
- Following, a random node is selected which needs to be searched by every other member in the chord ring exactly the number of times as per the message request parameter given by the user in the input. 
- We then create finger tables for each created node and perform stabilization upon them to calculate the average number of hops required for one particular node to search for another node.


**What is working?**
- The chord protocol is implemented using methods mentioned in the research paper provided in the problem statement. The model is working as expected. 
- There were several conflicts when creating distinct node IDs as a result of the hashing procedure as specified in the criteria proceeded by m bit reductions. In order to overcome this, the project incorporates a random number approach.
- The largest network tested with is 2000 nodes with 10 messages each, beyond which the program starts throwing multiple deadlock errors. The average number of hops required for a message to reach its targeted node was in the range of [2.6, 4.5] when tested from 100-2000 nodes. The value came out to be 3.45 hops when it was averaged across 20 runs. Hence, on an average it takes 3.45 hops for a node to search any other node in the chord protocol meaning the look-up time taken is in the order of log n.

## Observations:

| Number of Nodes | Average Hops |
| --------------- |:------------:|
| 100             | 2.6          |
| 500             | 3.2          |
| 1000            | 3.5          |
| 2000            | 4.5          |

**Running program for 1000 nodes, 10 message requests, 0 failure nodes:**
<img width="627" alt="Screen Shot 2022-10-23 at 6 23 36 PM" src="https://user-images.githubusercontent.com/59756917/197421129-a989feea-1712-4b35-99fa-711deb140597.png">

