# Project 1
**Group Members:**
1. Yash Goel, UFID - 51939756
2. Srikruth Reddy Puram, UFID - 70678514

**Outline:**

We have implemented a bitcoin mining simulation in Erlang using  the Actor Model. We used SHA256 algorithm to hash and mine bitcoins with the desired number of leading zeros. We have also implemented the Remote Configuration using client and server machines.

## Usage
1. cd the project folder
2. Start erl shell and compile both master and miner using c(master) and c(miner).
3. Run master.erl (Write erl -name master@[your_ip] -setcookie [any_cookie_name]).
4. Run miner.erl (Write erl -name miner@[your_ip] -setcookie [any_cookie_name]).
5. Ignore the warning anf follow the inputs generated by code to start mining.

## Implementation Details

1. To run the program locally, run the master.erl by following the above directions. Then you will be prompted with "Number of 0s to mine:" and "Number of miners to spawn:", fill the desired values and start mining.

2. To run the program on multiple machines start master.erl on one machine and pass 0 in "Number of miners to spawn:", then start miner.erl in other machines by following the usage guidelines you will get a prompt "Enter masters IP address with which you want to connect:", enter the host's IP here to connect and start mining.

**System 1: master.erl**

Before the miner.erl is run, the number of leading zeros (K) and the number of nodes on each system are entered into the master.erl (Miners).
The server produces miners (actors) to mine coins and then waits to hear from clients who want to participate in the mining or from the actors who were spawned to indicate that a coin was found, which the server will print. After receiving a message from a client, the server launches new miners to do continuous coin mining. Multiple customers can join at the same time. When a coin is discovered, these client-side actors respond in the same way as server-side actors and send a message to the latter. Each child actor is instructed to continue digging until the server discovers a given number of coins.

**System 2: miner.erl**

The client connects to the server by accepting the server node name as a parameter. The server then spawns actors on the client to begin mining for bitcoins.

## Assignment Details

**1. Work Unit:**

The job of creating a random string, computing the hash, and mining for bitcoins is given to each child actor. Each actor continues to execute until the target value is reached and the resulting hash is smaller than the minimum leading zero condition. All of the performers are put to death after discovering 100 coins. We came to the conclusion that each actor should be entirely responsible for creating the string, hashing it, and determining whether it is a genuine coin. Each actor receives a string for hashing from numerous miners. Similar to this, the server spent a considerable amount of time confirming each hash produced by these numerous actors, thus once a hash was discovered and transmitted to the server, the check for a genuine coin was shifted to the child actor.

In our program we give the user the option to enter the number of workers to spawn with an upper threshold of max 4 digit numerical(20,000). We specifically chose this work unit because:-
 
:This will avoid the possibility of repeated generation of the same string across the workers and limit it to the first n number of miners required by the user.

:Since different workers get different workloads this approach can be horizontally scalable

\


**2. Work Unit Metrics:**
We have defined the number of processes to run as = No. of Cores * 4. This ensures that all the cores are used efficiently to mine bitcoins in a faster manner.

**3. Result for 4 leading zeros on 4 miners spawned by server**

![Fig-1](https://user-images.githubusercontent.com/113138630/192105081-608c5b80-7e72-4bda-84c1-1fe69280476f.png)

*Fig.1 Starting the server to mine bitcoins with 4 trailing zeros by spawning 4 miners.*

![Fig-2](https://user-images.githubusercontent.com/113138630/192105369-a1e6cf0e-c4cc-4b31-9ff4-88526c0b982f.png)

*Fig.2 Miners start mining.*

![Fig-3](https://user-images.githubusercontent.com/113138630/192105386-546be252-c4cd-4395-9cb4-2936349ffeb8.png)

*Fig.3 CPU utilisation metrics.*

CPU time/ Run Time: 7.500693317099144

\

**4. Result for 5 leading zeros on miners spawned by both server and external client using IP address**

![Fig-4](https://user-images.githubusercontent.com/113138630/192105395-a0604309-7447-4857-bb18-14039551a930.png)

*Fig.4 Starting the server to mine bitcoins with 5 trailing zeros by spawning 2 miners on server and rest will be connected externally from different machine.*

![WhatsApp Image 2022-09-24 at 2 31 40 PM](https://user-images.githubusercontent.com/113138630/192113842-ec430298-0d93-410b-84fc-02c1ef9861bc.jpeg)

*Fig.5 Starting miner on different machine.*

![WhatsApp Image 2022-09-24 at 2 31 46 PM](https://user-images.githubusercontent.com/113138630/192113895-4fc9f8d8-7709-4e3c-a5aa-73651d5b29ba.jpeg)

*Fig.6 Server receives request from miner.*

![WhatsApp Image 2022-09-24 at 2 38 08 PM](https://user-images.githubusercontent.com/113138630/192113950-96d0af4c-6e50-4e77-8cd9-4371fe6b1502.jpeg)

*Fig.7 Starting 3 miners from different machine to connect to server.*


- <0.99.0> and <0.100.0> are miners hosted by server on machine 1.
- <14200.88.0> is miner hosted by machine 2.
- <14233.92.0> is miner hosted by machine 3.

-Total clock time: 468174.336

-Total CPU time: 861953

-CPU time/ Run Time: 1.841094083380085

\

**5. Largest coin found (i.e., the coin with the highest number of leading 0's)**

We found the largest coin to have seven zeros. The program was run for few hours to find 8 zeros, but no coins were found and the program was stopped.

![Fig-8](https://user-images.githubusercontent.com/113138630/192107638-7b7bca73-f091-4238-9257-fb5383975ee0.png)

*Fig.8 Largest coin found with highest leading zeros*
\
\


**6. Largest number of working machines**

We used Intel i7-8550U CPU (8 MB cache, 4 cores). We were able to run the script on four nodes but the same could be scaled to run on many more. We were able to connect three different machines to mine on 1 server.
  
  
