package com.plectix.testharness;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

public class TestHarness {
	
	private static XPath xpath = XPathFactory.newInstance().newXPath();
	
	public static void main(String[] args) {
		if (args.length < 1) {
			System.out.println("Please give the configuration file name as first argument");
			return;
		}
		
		Map<String, Long> executionTimes = new HashMap<String, Long>();
		
		InputSource inputSource = new InputSource(args[0]);
		
		List<Test> tests = getTests(inputSource);
		if (tests == null)
			return;

		Emailer emailer = parseEmailer(inputSource);
		if (emailer == null)
			return;
		
		List<List<String>> outputs = new ArrayList<List<String>>(); 
		
		for (Test test : tests) {
			try {
				System.out.println("Running test " + test.getName());
				long startTime = System.currentTimeMillis();
				BufferedReader bufferedReader = null;
				for (int i = 0; i < test.getNRuns(); i++) {
					Process process = Runtime.getRuntime().exec(test.getCommand());
					if (test.getOutput().equals("console")) {
						bufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream()));
					} else {
						bufferedReader = new BufferedReader(new FileReader(test.getOutput()));
					}
					process.waitFor();
				}
				executionTimes.put(test.getName(), System.currentTimeMillis() - startTime);
				outputs.add(getLines(bufferedReader));
				System.out.println("Test " + test.getName() + " executed");
			} catch (IOException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		String message = "";
		int size = tests.size();
		for (int i = 0; i < size; i++) {
			for (int j = 0; j < i; j++) {
				String differences = Diff.diff(outputs.get(i),  outputs.get(j));
				if (!differences.isEmpty()) {
					message += "Differences between " + tests.get(j).getName() + " and " + tests.get(i).getName() + " tests\n";
					message += differences;
				}
			}
		}
		
		System.out.println("Sending e-mails");
		
		if (!message.isEmpty()) {
			message = "Some tests failed:\n" + message;
		} else {
			message = "Tests run successfully\n";
		}
		message += "Time of execution:\n";
		for (String name: executionTimes.keySet()) {
			message += name + " : " + executionTimes.get(name) + "ms\n"; 
		}
		emailer.send(message);
		System.out.println("E-mails sent");
	}

	private static Emailer parseEmailer(InputSource inputSource) {
		try {
			String smtp = (String) xpath.evaluate("/testharness/emailer/@smtp", inputSource, XPathConstants.STRING);
			String from = (String) xpath.evaluate("/testharness/emailer/@from", inputSource, XPathConstants.STRING);
			List<String> recipients = new ArrayList<String>();
			NodeList nodes = (NodeList) xpath.evaluate("/testharness/emailer/recipient", inputSource,
					XPathConstants.NODESET);
			for (int i = 0; i < nodes.getLength(); i++) {
				recipients.add(nodes.item(i).getTextContent());
			}
			return new Emailer(smtp, recipients, from);
		} catch (XPathExpressionException e) {
			e.printStackTrace();
		}
		return null;
	}

	private static List<Test> getTests(InputSource inputSource) {
		List<Test> result = new ArrayList<Test>();
		try {
			NodeList nodes = (NodeList) xpath.evaluate("/testharness/tests/test", inputSource,
					XPathConstants.NODESET);
			for (int i = 0; i < nodes.getLength(); i++) {
				String command = nodes.item(i).getAttributes().getNamedItem("command").getNodeValue();
				String output = nodes.item(i).getAttributes().getNamedItem("output").getNodeValue();
				String name = nodes.item(i).getAttributes().getNamedItem("name").getNodeValue();
				int runs = Integer.parseInt(nodes.item(i).getAttributes().getNamedItem("runs").getNodeValue());
				result.add(new Test(command, output, name, runs));
			}
			return result;
		} catch (XPathExpressionException e) {
			e.printStackTrace();
		}
		return null;
	}

	private static List<String> getLines(BufferedReader bufferedReader) {
		try {
			List<String> result = new ArrayList<String>();
			String line;
			while ((line = bufferedReader.readLine()) != null) {
				result.add(line);
			}
			return result;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

}
