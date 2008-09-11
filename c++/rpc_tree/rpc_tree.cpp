// 
//  rpc_tree.cpp
//  rpc_tree
//  
//  Created by Lars Yencken on 2008-06-20.
//  Copyright 2008-06-20 Lars Yencken. All rights reserved.
// 

#include <vector>
#include <string>
#include <iostream>

using namespace std;

//-------------------------------------------------------------------------//
// PROBLEM SPEC
// 
// A particular set of servers is arranged in a tree. Each server can be
// queried by name for a list of its children. Write a get_count() method
// which takes the name of the root server, determines the total number of 
// servers in the tree, and prints the number out.
// 
// Bonus points if the function can be made non-blocking.
//-------------------------------------------------------------------------//

/**
 * A callback interface for remote procedure calls.
 */
class RpcReceiver
{
public:
    virtual void rpc_reply(vector<string> children);
};

/**
 * The remote procedure call interface. Note that it is non-blocking.
 */
void rpc_info(string server_name, RpcReceiver* receiver);

/**
 * Interface for the solution which must be written.
 */
void get_count(string root_server);

//-------------------------------------------------------------------------//
// PROBLEM SOLUTION
//-------------------------------------------------------------------------//

/**
 * Keep a count of the number of outstanding requests, and of the current
 * total. When the last request comes in, print the total.
 */
class CountReceiver : RpcReceiver
{
public:
    CountReceiver(string root_server)
    {
        m_count = 1;
        m_waiting = 1;
        rpc_info(root_server, this);    
    }
    
    virtual void rpc_reply(vector<string> children)
    {
        m_count += children.size();
        m_waiting += children.size() - 1;
        for (int i = 0; i < children.size(); i++)
        {
            rpc_info(children[i], this);
        }
        if (m_waiting == 0)
        {
            cout << m_count << endl;
        }
    }
    
private:
    int m_count;
    int m_waiting;
};

/**
 * To initiate the counting, we just instantiate a new object.
 */
void get_count(string root_server)
{
    CountReceiver* c = new CountReceiver(root_server);
}