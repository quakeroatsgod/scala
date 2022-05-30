/*
*   This is a refresher of how to write scala code. More importantly, This is my final project from
*   CS 355 that has been remade to make it more robust. It is the same idea as Scenario 1, except 
*   with file I/O, less hard-coding, and more
*    dfi 1 5 3 1 =10
*    Fastest Path on given graph:   dfi
*/
import scala.io.Source
import java.io.FileNotFoundException
object Main{
    //Node class that we can use to build the graph. Each node has a name as well
    //as three node tuples that it "points" to. Each tuple has an integer cost
    case class Node(var name: String, var paths: Set[(Node, Integer)], var marked: Boolean)

    //This method creates and returns an unmarked node with no paths or costs.
    //A name is given to it
    def makeNode(name:String): Node=Node(name,Set(),false)

    //This method adds a node to the graph. It is recursive
    def graphInsert(startNode: Node, newNode: Node, path: Map[String,Integer]): Unit={
        //Base case. Leave immediately if the node we are in doesn't actually exist.
        if(startNode==null) return;
        //Make sure the node does not point to itself
        //TODO maybe add in and/or change edge case?
        //assert(!startNode.name.equals(newNode.name))
        //If the current node has the name of the node in the path list, insert it into the graph
        if(path.contains(startNode.name) && !startNode.paths.contains((newNode,path.apply(startNode.name)))){
            //Insert the node path. Duplcates are not inserted
            startNode.paths++=Set((newNode,path.apply(startNode.name)))
        }
        //Recursively traverse the graph in the event that we need to go further to make a node
        for(nodePath <- startNode.paths){
            graphInsert(nodePath._1,newNode,path)
        }  
    }
    
    //This method prints out each node on the graph and where they point to It is recursive
    def printGraph(startNode: Node): Unit={
        //Base case
        if(startNode==null) return;
        //We mark the node so we don't print it more than once
        if(!startNode.marked){
            print("Node Name: "+startNode.name+" \t\tPoints to: ")
            for(nodePath <- startNode.paths){
                print(nodePath._1.name+" ")
            }
            println("")
            startNode.marked=true;
        }
        //Recursively traverse to each accessible node that isn't null
        for(nodePath <- startNode.paths){
            printGraph(nodePath._1)
        }
    }
    //This method clears all the marked flags in the graph
    def unmarkNodes(startNode: Node): Unit={
        //Base case
        if(startNode==null) return;
        if(startNode.marked){
            startNode.marked=false
            for(nodePath <- startNode.paths){
                printGraph(nodePath._1)
            }
        }
    }

    //This function creates the entire graph. It consists of several calls to
    //graphInsert, which inserts a node with a specific path and cost
    //the node homeNode is created as sentinel/start/root node of the graph.
    //At the end, homeNode is returned.
    def makeGivenGraph(): Node={
        var homeNode: Node=makeNode("home");
        graphInsert(homeNode,makeNode("a"), Map("home"->1) )
        graphInsert(homeNode,makeNode("b"), Map("a"->1) )
        graphInsert(homeNode,makeNode("c"), Map("home"->0,"b"->2) )
        graphInsert(homeNode,makeNode("d"), Map("home"->1) )
        graphInsert(homeNode,makeNode("e"), Map("a"->2,"b"->7,"c"->6) )
        graphInsert(homeNode,makeNode("f"), Map("e"->4,"d"->5) )
        graphInsert(homeNode,makeNode("g"), Map("f"->0,"d"->8) )
        graphInsert(homeNode,makeNode("h"), Map("e"->3) )
        graphInsert(homeNode,makeNode("i"), Map("f"->3,"g"->3) )
        graphInsert(homeNode,makeNode("office"), Map("h"->8,"i"->1) )
        return homeNode
    }
    
    //This methods calculates dijkstra's algorithm for a given starting node and
    //Ending node name. If a path exists, the algorithm traverses it and acquires the cost
    //of the path to  the end node. The algorithm moves to the end node immediately,
    //then at each step-up iteration, it compares the cost of each of the three paths
    //it can take. The method returns a tuple consisting of the cost,a list of the node path, and 
    //the total number of nodes traversed so far
    def dijkstra(startNode: Node, finalNodeName: String): (Integer,List[String], Integer)={
        //Base case. Don't add null paths or if we reach the end of the path
        if(startNode.name==finalNodeName){
            //return base cost
            return (0,List(finalNodeName),1)
        }
        //Initialization values. The -1 is so that no matter the cost, there will not be any
        //bugs in reference to the cost/nodes traveled to being too big
        var leastCost: (Integer,List[String],Integer)= (-1, Nil,-1)
        var currentPath: (Integer,List[String],Integer)=(0,Nil,0)
        //If there is a path to a node
        for(nodePath <- startNode.paths){
            //Copy the path values recursively
            currentPath=dijkstra(nodePath._1,finalNodeName)
            //Cost should never be less than 0
            assert(currentPath._1 >= 0)
            if(leastCost._1==(-1)){
                leastCost=((currentPath._1+ nodePath._2) , currentPath._2, currentPath._3)
            }
            //If the path cost and nodes traveled is less than the current least cost path, use these path values
            else if((leastCost._1 >= (currentPath._1 + nodePath._2)) && (leastCost._3 >= currentPath._3)){   
                leastCost=((currentPath._1+ nodePath._2) , currentPath._2, currentPath._3)
            }
        }
        //Append the new least cost, list of nodes, and number of nodes traveled
        return(leastCost._1,startNode.name::leastCost._2,leastCost._3+1)
    }

    //Splits a line from a file into an array of strings
    def parseFile(unparsedLine:String): Array[String]=unparsedLine.split(" ")

    def buildMap(parsedLine:Array[String]): Map[String,Integer]={
        var mapBuilder: Map[String,Integer]=Map()
        for(i<-Range.inclusive(1,parsedLine.length-1,2)){
            //No, += does not work. Appends a new key to the path map
            mapBuilder=mapBuilder+(parsedLine(i)->parsedLine(i+1).toInt)
        }
        return mapBuilder
    }
    
    //Makes the graph data structure based on the graph from the file provided. The start and end nodes
    //are returned.
    def makeGraphFromFile(filepath:String):(Node,Node)={
        var startEndNodes: (Node,Node)=(null,null)
        var node: Node= null
        var startFlag: Boolean=false
        var pathMap: Map[String,Integer]=null
        try{
            Source.fromFile(filepath).getLines().foreach{
                //Lambda expression to iterate through each of lines in the file
                unparsedLine=>{
                    if(unparsedLine.length > 0){
                        val parsedLine=parseFile(unparsedLine)
                        //simple trick to make sure there are an odd number of elements in the split,
                        //meaning there are a valid number of arguments
                        assert((parsedLine.length % 2 == 1))
                        //Format for parsing from file: <node_name, from_node, path_cost, from_node, path_cost...>
                        //The first line MUST be the starting node, and the last line MUST be the final node
                        try{
                            //Map to build the node paths from the file
                            pathMap=buildMap(parsedLine)
                            //New blank node to insert into the graph
                            node=makeNode(parsedLine(0))
                        } catch {
                            case e: NumberFormatException => {
                                println("Error, invalid file contents")
                                System.exit(0)}
                        }
                        if(!startFlag){
                            startEndNodes=(node,null)
                            startFlag=true
                        }
                        //Insert new node into the graph
                        graphInsert(startEndNodes._1,node,pathMap)
                    }
                }
            }
        }   
        catch{
            case e: FileNotFoundException => {
                println("Error Reading From file")
                System.exit(0)}
        }
        //Save start and end nodes
        startEndNodes=(startEndNodes._1,node)
        return startEndNodes
    }
    //Main program method
    def main(args:Array[String]): Unit={
        var fileFlag: Boolean=false
        var startNode: Node=null
        var shortestPath: (Integer,List[String], Integer)=(0,Nil,0)
        args.length match{
            //use given graph inputs
            case 0 => 
                println("No given filepath to graph, using pre-made graph from the assignment")
                startNode=makeGivenGraph()
                shortestPath=dijkstra(startNode,"office")
            //Graph provided, use that
            case 1 => 
                fileFlag=true
                println("Using filepath <"+args(0)+">")
                val startEndNodes: (Node,Node)= makeGraphFromFile(args(0))
                startNode=startEndNodes._1
                shortestPath=dijkstra(startEndNodes._1,startEndNodes._2.name)
            //base case
            case _ => 
                println("Usage: ./main <Optional filepath>")
                return
        }
        printGraph(startNode)
        print("Shortest path on given graph: ")
        for(node <- shortestPath._2){
            print(node)
            if(node!=shortestPath._2.last) print(" -> ")
        }
        println("\nCost of path: "+shortestPath._1)
        println("Total number of nodes traversed: "+shortestPath._3)
    }
}