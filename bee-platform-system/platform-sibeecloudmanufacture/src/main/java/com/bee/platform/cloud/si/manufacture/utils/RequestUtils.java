package com.bee.platform.cloud.si.manufacture.utils;

import net.sf.json.JSONObject;
import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.apache.commons.httpclient.params.HttpMethodParams;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Map;
import java.util.Map.Entry;


/**
 * @Description 请求处理类
 * @author chenxm66777123
 * @Date 2018年12月20日
 * @version 1.0.0
 */
public class RequestUtils {

	private static HttpURLConnection conn;
	private static OutputStream os = null;
	private static BufferedReader br = null;

	/**
	 * @Description httpGet请求
	 * @author chenxm66777123
	 * @Date 2018年12月23日
	 * @version 1.0.0
	 */
    public static String sendGet(String url, Object param) {
        String result = "";      
        BufferedReader in = null;
        PrintWriter out = null;
        try {
            URL realUrl = new URL(url);
            // 打开和URL之间的连接
            URLConnection connection = realUrl.openConnection();
            connection.setDoOutput(true);
            // 建立实际的连接
            connection.connect();
            out = new PrintWriter(connection.getOutputStream());
            // 发送请求参数
            out.print(param);
            // 定义 BufferedReader输入流来读取URL的响应
            in = new BufferedReader(new InputStreamReader(
                    connection.getInputStream(), "UTF-8"));
            String line;
            while ((line = in.readLine()) != null) {
                result += line;
            }
        } catch (Exception e) {
            System.out.println("发送GET请求出现异常!" + e);
            e.printStackTrace();
        }
        // 使用finally块来关闭输入流
        finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (Exception e2) {
                e2.printStackTrace();
            }
			try {
				if (out != null) {
					out.close();
				}
			} catch (Exception e2) {
				e2.printStackTrace();
			}
        }
        return result;
    }

    
    /**
	 * @Description httpPost请求(form请求)
	 * @author chenxm66777123
	 * @Date 2018年12月23日
	 * @version 1.0.0
	 */
    public static String sendPostForForm(Map<String, String> params, String path,int timeOut) {
		int retryCount = 1;

		try {
			 // 构建请求参数  
	        StringBuffer sb = new StringBuffer();  
	        if (params != null) {  
	            for (Entry<String, String> e : params.entrySet()) {  
	                sb.append(e.getKey());  
	                sb.append("=");  
	                sb.append(e.getValue());  
	                sb.append("&");  
	            }  
	            sb.substring(0, sb.length() - 1);  
	        }  
			byte[] b = sb.toString().getBytes("UTF-8");
			HttpClient httpClient = new HttpClient();
			httpClient.getParams().setBooleanParameter("http.protocol.expect-continue", false);
			PostMethod webMethod = new PostMethod(path);
			webMethod.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,new DefaultHttpMethodRetryHandler(retryCount, false));
			InputStream is = new ByteArrayInputStream(b, 0, b.length);
			RequestEntity re = new InputStreamRequestEntity(is, b.length,"application/x-javascrip; charset=utf-8");
			webMethod.setRequestEntity(re);
			webMethod.setRequestHeader("Accept-Encoding", "UTF-8");
			webMethod.setRequestHeader("Content-Type","application/x-www-form-urlencoded");
			httpClient.getHttpConnectionManager().getParams().setConnectionTimeout(timeOut);
			httpClient.getParams().setConnectionManagerTimeout(timeOut);
			httpClient.getParams().setSoTimeout(timeOut);
			httpClient.executeMethod(webMethod);
			BufferedReader in = new BufferedReader(new InputStreamReader(webMethod.getResponseBodyAsStream(), "UTF-8"));
			
			String line;
			StringBuilder returnsb = new StringBuilder();
			// 循环读取流
			while ((line = in.readLine()) != null) {
				returnsb.append(line);
			}
			in.close();
			// 释放连接
			if (webMethod != null) {
				webMethod.releaseConnection();
			}
			String responseSOAP = returnsb.toString();
			return responseSOAP;
		} catch (Exception e) {
			return e.getMessage();
		}
    }
    
    /**
	 * @Description httpPost请求(application/json请求)
	 * @author chenxm66777123
	 * @Date 2018年12月23日
	 * @version 1.0.0
	 */
    public static JSONObject sendPostForJson(String sendJosn, String path,int timeOut) {
		int retryCount = 1;

		try {
			String requestSOAP = sendJosn;
			byte[] b = requestSOAP.getBytes("UTF-8");
			HttpClient httpClient = new HttpClient();
			httpClient.getParams().setBooleanParameter("http.protocol.expect-continue", false);
			PostMethod webMethod = new PostMethod(path);
			webMethod.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,new DefaultHttpMethodRetryHandler(retryCount, false));
			InputStream is = new ByteArrayInputStream(b, 0, b.length);
			RequestEntity re = new InputStreamRequestEntity(is, b.length,"application/x-javascrip; charset=utf-8");
			webMethod.setRequestEntity(re);
			webMethod.setRequestHeader("Accept-Encoding", "UTF-8");
			webMethod.setRequestHeader("Content-Type","application/json;charset=UTF-8");
			httpClient.getHttpConnectionManager().getParams().setConnectionTimeout(timeOut);
			httpClient.getParams().setConnectionManagerTimeout(timeOut);
			httpClient.getParams().setSoTimeout(timeOut);
			httpClient.executeMethod(webMethod);
			BufferedReader in = new BufferedReader(new InputStreamReader(webMethod.getResponseBodyAsStream(), "UTF-8"));
			
			String line;
			StringBuilder sb = new StringBuilder();
			// 循环读取流
			while ((line = in.readLine()) != null) {
				sb.append(line);
			}
			in.close();
			// 释放连接
			if (webMethod != null) {
				webMethod.releaseConnection();
			}
			String responseSOAP = sb.toString();
			return JSONObject.fromObject(responseSOAP);
		} catch (Exception e) {
			JSONObject json = new JSONObject();
			json.put("errcode", e.getMessage());
			return json;
		}
	}

	public static String sendPostPrint(String url,String param) {
		String str = "";
		try {
			URL u = new URL(url);
			// 打开和URL之间的连接
			conn = (HttpURLConnection) u.openConnection();
			// 设置允许写出数据,默认是不允许 false
			conn.setDoOutput(true);
			//当前的连接可以从服务器读取内容, 默认是true
			conn.setDoInput(true);
			conn.setRequestMethod("POST");
			// 获取URLConnection对象对应的输出流
			os = conn.getOutputStream();
			// 发送请求参数
			os.write(param.getBytes());
			os.flush();
			str = getResponse();
		} catch (Exception e) {
			System.out.println("发送 POST 请求出现异常！" + e);
			e.printStackTrace();
		}
		//使用finally块来关闭输出流、输入流
		finally {
			try {
				if (os != null) {
					os.close();
				}
				if (br != null) {
					br.close();
				}
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
		return str;
	}

	public static String sendGetPrint(String url,String param) {
		String result = "";
		BufferedReader in = null;
		try {
			String urlNameString = url + "?" + param;
			URL realUrl = new URL(urlNameString);
			// 打开和URL之间的连接
			URLConnection connection = realUrl.openConnection();
			// 设置通用的请求属性
			connection.setRequestProperty("accept", "*/*");
			connection.setRequestProperty("connection", "Keep-Alive");
			connection.setRequestProperty("user-agent",
					"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)");
			// 建立实际的连接
			connection.connect();
			// 定义 BufferedReader输入流来读取URL的响应
			in = new BufferedReader(new InputStreamReader(
					connection.getInputStream()));
			String line;
			while ((line = in.readLine()) != null) {
				result += line;
			}
		} catch (Exception e) {
			System.out.println("发送GET请求出现异常！" + e);
			e.printStackTrace();
		}
		// 使用finally块来关闭输入流
		finally {
			try {
				if (in != null) {
					in.close();
				}
			} catch (Exception e2) {
				e2.printStackTrace();
			}
		}
		return result;
	}

	private static String getResponse() {
		String res = "";
		try {
			br = new BufferedReader(new InputStreamReader(conn.getInputStream(), "utf-8"));
			res = br.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return res;
	}
}
