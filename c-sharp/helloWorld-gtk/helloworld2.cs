//
// helloworld.cs - Gtk# Tutorial example
//
 
using Gtk;
using GtkSharp;
using System;
using System.Drawing;
 
public class HelloWorld {
	public static void Main(string[] args)
	{
		// This is called in all GTK applications. Arguments are parsed
		// from the command line and are returned to the application. */
		Application.Init();
 
		// create a new window
		Window window = new Window("My first GTK# application!");
 
		// When the window is given the "delete_event" signal (this is given
		// by the window manager, usually by the "close" option, or on the
		// titlebar), we ask it to call the delete_event () function
		// as defined above. The data passed to the callback
		// function is NULL and is ignored in the callback function.
		window.Resize(200,200);
    
        Label myLabel = new Label();

        myLabel.Text = "Hello World!!!!";
 
		// This packs the button into the window (a gtk container).
		window.Add(myLabel);
 
		// The final step is to display this newly created 
		window.ShowAll();
 
		// All GTK applications must call the main loop: Application.Run
		// Events are processed and dispatched here.
 
		Application.Run();
	}
}
