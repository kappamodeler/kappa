package com.plectix.testharness;

public class Test {
	public String getName() {
		return name;
	}

	private String name;
	private String command;
	private String output;
	private int nRuns;

	public Test(String command, String output, String name, int runs) {
		this.command = command;
		this.output = output;
		this.name = name;
		this.nRuns = runs; 
	}

	
	public String getCommand() {
		return command;
	}
	
	public int getNRuns() {
		return nRuns;
	}


	public String getOutput() {
		return output;
	}
	
}