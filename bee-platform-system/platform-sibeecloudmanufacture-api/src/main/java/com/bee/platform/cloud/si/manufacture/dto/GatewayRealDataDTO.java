package com.bee.platform.cloud.si.manufacture.dto;

import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @description: 网关每次采集的数据
 * @author: junyang.li
 * @create: 2019-10-10 17:03
 **/
@Data
@ToString
@Accessors(chain = true)
public class GatewayRealDataDTO implements Serializable {

    private static final long serialVersionUID = 842274164299647790L;
    /**
     * 网关编号
     */
    private String hcGatewayId;
    /**
     * 返回的实时数据
     */
    private Map<String,String> map;
    /**
     * 返回的实时数据
     */
    private List<PlcRealDataDTO> list;
    /**
     * 采集时间
     */
    private Date receiveTime;

    public GatewayRealDataDTO(String hcGatewayId, Map<String,String> map, Date receiveTime) {
        this.hcGatewayId = hcGatewayId;
        this.map = map;
        this.receiveTime = receiveTime;
    }
}
