package com.plectix.testharness;

import java.util.List;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class Emailer {
	
	private String smtpHost;
	private List<String> recipients;
	private String fromAddress;

	public Emailer(String smtpHost, List<String> recipients, String fromAddress) {
		this.smtpHost = smtpHost;
		this.recipients = recipients;
		this.fromAddress = fromAddress;
	}

	public void send(String text) {
		try {
			Properties props = new Properties();
			props.put("mail.smtp.host", smtpHost);
			
			Session mailSession = Session.getInstance(props);
			
			MimeMessage msg = new MimeMessage(mailSession);
			msg.setFrom(new InternetAddress(fromAddress));
			InternetAddress[] addressTo = new InternetAddress[recipients.size()]; 
			int i = 0;
			for (String recipient : recipients) {
				addressTo[i++] = new InternetAddress(recipient);
			}
			    
		    msg.setRecipients(Message.RecipientType.TO, addressTo);
			msg.setSubject("Testharness notification");
		    msg.setText(text);
				
		    Transport.send(msg);
		} catch (AddressException e) {
			e.printStackTrace();
		} catch (MessagingException e) {
			e.printStackTrace();
		}
		
	}


} 


