package com.bee.platform.cloud.si.manufacture.socket;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ByteToMessageDecoder;

import java.util.List;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 自定义消息解析器
 * @Date 2019/9/24 17:14
 */
public class MessagePacketDecoder extends ByteToMessageDecoder {

    /**
     * @Description 自定义解码操作
     * @author chenxm66777123
     * @Date 2019/9/26 13:49
     * @version 1.0.0
     */
    @Override
    protected void decode(ChannelHandlerContext ctx, ByteBuf buffer, List<Object> out) throws Exception {
        //创建字节数组,buffer.readableBytes可读字节长度
        byte[] b = new byte[buffer.readableBytes()];
        //复制内容到字节数组b
        buffer.readBytes(b);

        out.add(bytesToHexString(b));
    }

    /**
     * @Description 16进制数组数据转为16进制字符串
     * @author chenxm66777123
     * @Date 2019/9/26 13:49
     * @version 1.0.0
     */
    public String bytesToHexString(byte[] bArray) {
        StringBuffer sb = new StringBuffer(bArray.length);
        String sTemp;
        for (int i = 0; i < bArray.length; i++) {
            sTemp = Integer.toHexString(0xFF & bArray[i]);
            if (sTemp.length() < 2) {
                sb.append(0);
            }
            sb.append(sTemp.toUpperCase());
        }
        return sb.toString();
    }

}
