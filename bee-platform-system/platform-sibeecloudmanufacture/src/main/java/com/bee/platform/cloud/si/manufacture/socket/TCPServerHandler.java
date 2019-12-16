package com.bee.platform.cloud.si.manufacture.socket;

import com.bee.platform.cloud.si.manufacture.dao.mapper.WeightMachineDataMapper;
import com.bee.platform.cloud.si.manufacture.entity.WeightMachineData;
import com.bee.platform.cloud.si.manufacture.utils.SpringUtils;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.ReferenceCountUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description tcp数据处理
 * @Date 2019/9/24 17:15
 */
@Slf4j
public class TCPServerHandler extends ChannelInboundHandlerAdapter {

    private static WeightMachineDataMapper weightMachineDataMapper;
    static {
        weightMachineDataMapper = SpringUtils.getBean(WeightMachineDataMapper.class);
    }

    public TCPServerHandler() {

    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object source) throws Exception {
        try {
            String body = (String) source;
            //选找到第一个3D的位置
            Integer index = body.indexOf("3D");
            //截取到2E -  3D 之间数据
            String data = body.substring(4, index);
            //处理数据 16进制转字符串
            data =  SocketUtils.convertHexToString(data);
            //反转数据
            data =  SocketUtils.reverseString(data);
            //入库数据
            Integer inData = Integer.valueOf(data);

            //截取设备编码
            String deviceId = body.substring(index + 2);
            if(inData != null && inData != 0){
                //入库操作
                WeightMachineData buyWeightMachineData = new WeightMachineData();
                buyWeightMachineData.setDeviceId(deviceId);
                buyWeightMachineData.setWeightData(String.valueOf(inData));
                buyWeightMachineData.setCreateTime(new Date());
                weightMachineDataMapper.insert(buyWeightMachineData);
            }
            ReferenceCountUtil.release(source);
        }
        catch (Exception e){
            log.info("地磅数据读取报错！！ : {}",e.getMessage());
        }
    }


}