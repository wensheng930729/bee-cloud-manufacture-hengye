package com.bee.platform.cloud.si.manufacture.dto;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description: 盘点单列表查询参数
 * @author: junyang.li
 * @create: 2019-11-27 14:45
 **/
@Data
@Accessors(chain = true)
public class StockInventoryListParam implements Serializable {

    private static final long serialVersionUID = 1524328982175424985L;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 查询开始时间
     */
    private Date startTime;
    /**
     * 查询结束时间
     */
    private Date endTime;

    public StockInventoryListParam() {
    }

    public StockInventoryListParam(Integer enterpriseId, Integer factoryId, Date startTime, Date endTime) {
        this.enterpriseId = enterpriseId;
        this.factoryId = factoryId;
        this.startTime = startTime;
        this.endTime = endTime;
    }
}
