package kata

import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http._
import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.client.entity.UrlEncodedFormEntity

import org.apache.http.entity.StringEntity
import org.apache.http.message.BasicNameValuePair
import org.apache.http.protocol.{HTTP, BasicHttpContext}
import org.apache.http.params.{HttpParams, HttpProtocolParams, BasicHttpParams}
import org.apache.http.util.EntityUtils

import java.io.{ InputStream, OutputStream }


class SimpleHttp(host: Option[HttpHost]) extends DefaultHttpClient {
  def this() = this(None)
  
  def this(host: HttpHost) = this(Some(host))
  
  def this(hostname: String) = this(new HttpHost(hostname))
  
  def this(hostname: String, port: Int) = this(new HttpHost(hostname, port))
  
  override def createHttpParams(): HttpParams = {
    val params = new BasicHttpParams
    HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1)
    HttpProtocolParams.setContentCharset(params, HTTP.UTF_8)
    HttpProtocolParams.setUseExpectContinue(params, false)
    params
  }
  
  /** Executes a request. */
  def x[T](request: HttpUriRequest) = new {
    def apply(thunk: HttpResponse => T): T = {
      val response = host match {
        case None       => execute(request)
        case Some(host) => execute(host, request)
      }
    
      response.getEntity match {
        case null   => error(response.getStatusLine.toString)
        case entity => 
          try {
            thunk(response)
          } finally {
            entity.consumeContent
          }
      }
    }
    
    def when(predicate: Int => Boolean)(thunk: HttpResponse => T) =
      apply { response =>
        if (predicate(response.getStatusLine.getStatusCode))
          thunk(response)
        else
          error(response.toString)
      }
    
    def whenOk = (this when { code => code >= 200 && code < 204 }) _
  }
  
  def apply(uri: String): Request = new Request(uri)
  
  def get(uri: String): Response = new Response(new HttpGet(uri))
  
  class Response(request: HttpUriRequest) {
    def >>[T](thunk: InputStream => T): T =
      x(request) whenOk { response => thunk(response.getEntity.getContent) }
    
    def >>>(out: OutputStream): Unit = x(request) whenOk { _.getEntity writeTo out }
    
    def withResponse[T](thunk: HttpResponse => T): T = x(request)(thunk)
  }
  
  class Request(uri: String) {
    def <<<(body: Any) = {
      val request = new HttpPut(uri)
      request setEntity new StringEntity(body.toString, HTTP.UTF_8)
      HttpProtocolParams.setUseExpectContinue(request.getParams, false)
      new Response(request)
    }
    
    def <<(values: (String, Any)*) = {
      val request = new HttpPost(uri)
      request setEntity new UrlEncodedFormEntity(
        java.util.Arrays.asList(
          (values map { pair => new BasicNameValuePair(pair._1, pair._2.toString) }): _*
        ),
        HTTP.UTF_8)
      new Response(request)
    }
  }
}

import org.apache.http.conn.scheme.{Scheme,SchemeRegistry,PlainSocketFactory}
import org.apache.http.conn.ssl.SSLSocketFactory
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager

object SimpleHttp extends SimpleHttp {
  override def createClientConnectionManager() = {
    val registry = new SchemeRegistry()
    registry register new Scheme("http", PlainSocketFactory.getSocketFactory(), 80)
    registry register new Scheme("https", SSLSocketFactory.getSocketFactory(), 443)
    new ThreadSafeClientConnManager(getParams(), registry)
  }
}
