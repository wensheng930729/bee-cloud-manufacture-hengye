package com.bee.platform.cloud.si.manufacture.socket;

import com.bee.platform.cloud.si.manufacture.dao.repository.ElasticSearchRepository;
import com.bee.platform.cloud.si.manufacture.entity.ElectricityMeterData;
import com.bee.platform.cloud.si.manufacture.utils.SpringUtils;
import com.bee.platform.cloud.si.manufacture.utils.XMLReaderUtils;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.CharsetUtil;
import lombok.extern.slf4j.Slf4j;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;

/**
 * @ClassName: MeterDataServerHandler
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/24 14:55
 * @Version: 1.0
 */
@Slf4j
public class MeterDataServerHandler extends ChannelInboundHandlerAdapter {

    private static ElasticSearchRepository elasticSearchRepository;

    static {
        elasticSearchRepository = SpringUtils.getBean(ElasticSearchRepository.class);
    }

    /*public MeterDataServerHandler() {
    }*/

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object source) {
        ByteBuf buf = (ByteBuf) source;
        String xmlStr = buf.toString((CharsetUtil.UTF_8));
        log.info("电表body数据为:  {} ", xmlStr);
        try {
            Document document = DocumentHelper.parseText(xmlStr);
            ElectricityMeterData electricityMeterData = XMLReaderUtils.read(document);
            System.out.println("解析后的数据为 ： "+electricityMeterData);
            //保存到es
            elasticSearchRepository.insert(ElectricityMeterData.class,electricityMeterData);
        } catch (Exception e) {
            log.error("转换xml格式异常,接收到的数据为 ： {}，异常信息 : {}",xmlStr,e);
        }

    }
}
