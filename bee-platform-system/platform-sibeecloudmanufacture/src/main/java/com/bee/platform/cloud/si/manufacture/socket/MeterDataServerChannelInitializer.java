package com.bee.platform.cloud.si.manufacture.socket;

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandler;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.SocketChannel;
import io.netty.handler.codec.DelimiterBasedFrameDecoder;
import io.netty.handler.timeout.IdleStateHandler;
import io.netty.util.concurrent.DefaultEventExecutorGroup;
import io.netty.util.concurrent.EventExecutorGroup;

import java.util.concurrent.TimeUnit;

/**
 * @ClassName: MeterDataServerChannelInitializer
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/24 14:53
 * @Version: 1.0
 */
@ChannelHandler.Sharable
public class MeterDataServerChannelInitializer extends ChannelInitializer<SocketChannel> {
    static final EventExecutorGroup group = new DefaultEventExecutorGroup(2);

    public MeterDataServerChannelInitializer() throws InterruptedException {
    }

    @Override
    protected void initChannel(SocketChannel socketChannel) throws Exception {

        ChannelPipeline pipeline = socketChannel.pipeline();

        //Tcp粘包处理--包尾加特殊字符处理
//        String delimiter = "$";
        // 将delimiter设置到DelimiterBasedFrameDecoder中，经过该解码一器进行处理之后，源数据将会
        // 被按照$进行分隔，这里1024指的是分隔的最大长度，即当读取到1024个字节的数据之后，若还是未
        // 读取到分隔符，则舍弃当前数据段，因为其很有可能是由于码流紊乱造成的
//        pipeline.addLast(new DelimiterBasedFrameDecoder(1024,
//                Unpooled.wrappedBuffer(delimiter.getBytes())));

        // 将分隔之后的字节数据转换为字符串数据
//        pipeline.addLast(new MessagePacketDecoder());

        // 这是我们自定义的一个编码器，主要作用是在返回的响应数据最后添加分隔符
//        pipeline.addLast(new DelimiterBasedFrameEncoder(delimiter));

        //IdleStateHandler心跳机制,如果超时触发Handle中userEventTrigger()方法
        pipeline.addLast("idleStateHandler",
                new IdleStateHandler(15, 0, 0, TimeUnit.MINUTES));

        //自定义Hadler
        pipeline.addLast("handler",new MeterDataServerHandler());

        //自定义Hander,可用于处理耗时操作，不阻塞IO处理线程
        //pipeline.addLast(group,"BussinessHandler",null);
    }

}
