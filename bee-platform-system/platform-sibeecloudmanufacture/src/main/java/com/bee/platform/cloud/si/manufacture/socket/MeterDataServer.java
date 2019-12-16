package com.bee.platform.cloud.si.manufacture.socket;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @ClassName: MeterDataServer
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/24 14:35
 * @Version: 1.0
 */

@Slf4j
@Component
public class MeterDataServer {
    private static MeterDataServer tcpServer;
    /**
     * 端口号
     **/
    private int port = 15681;
    /**
     * 服务器运行状态
     **/
    private volatile boolean isRunning = false;
    /**
     * 处理Accept连接事件的线程，这里线程数设置为1即可，netty处理链接事件默认为单线程，过度设置反而浪费cpu资源
     **/
    private EventLoopGroup bossGroup;
    /**
     * 处理hadnler的工作线程，其实也就是处理IO读写 。线程数据默认为 CPU 核心数乘以2
     **/
    private EventLoopGroup workerGroup;
    private ServerBootstrap serverBootstrap;
    private ChannelFuture channelFuture;

    private MeterDataServer() {
        //创建主线程池
        this.bossGroup = new NioEventLoopGroup(1);
        //创建从线程池
        this.workerGroup = new NioEventLoopGroup();
        this.serverBootstrap = new ServerBootstrap();
        try {
            serverBootstrap.group(bossGroup, workerGroup).
                    channel(NioServerSocketChannel.class).childHandler(new MeterDataServerChannelInitializer());
        } catch (Exception e) {

        }
    }

    public synchronized static MeterDataServer getInstance() {
        if (tcpServer == null) {
            tcpServer = new MeterDataServer();
        }
        return tcpServer;
    }

    /**
     * 服务启动
     */
    public synchronized void startServer() {
        try {
            this.init();
        } catch (Exception ex) {

        }
    }

    public void init() throws Exception {
        //标识当服务器请求处理线程全满时，用于临时存放已完成三次握手的请求的队列的最大长度
        serverBootstrap.option(ChannelOption.SO_BACKLOG, 1024);
        // 是否启用心跳保活机机制
        serverBootstrap.childOption(ChannelOption.SO_KEEPALIVE, true);
        log.info("电表采集TCP启动端口: {}  ", port);
        //绑定端口后，开启监听
        this.channelFuture = serverBootstrap.bind(port).syncUninterruptibly();
        if(channelFuture.isSuccess()){
            log.info("电表采集TCP程序启动成功！！！！！");
        }
        channelFuture.channel().closeFuture();
    }

    /**
     * 服务关闭
     */
    public synchronized void stopServer() {
        if (!this.isRunning) {
            throw new IllegalStateException(this.getName() + " 未启动 .");
        }
        this.isRunning = false;
        try {
            this.workerGroup.shutdownGracefully().await();
            this.bossGroup.shutdownGracefully().await();

        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        log.info("TCP服务已经停止...");
    }

    private String getName() {
        return "TCP-Server";
    }
}
