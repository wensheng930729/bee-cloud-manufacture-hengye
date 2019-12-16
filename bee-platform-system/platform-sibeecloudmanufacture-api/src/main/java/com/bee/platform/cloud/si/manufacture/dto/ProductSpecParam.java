package com.bee.platform.cloud.si.manufacture.dto;

import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @description: 产品规格参数查询
 * @author: junyang.li
 * @create: 2019-11-26 15:58
 **/
@Data
@Accessors(chain = true)
@ToString
public class ProductSpecParam {

    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 产品类别id
     */
    private List<Integer> categoryIds;
    /**
     * 产品id
     */
    private List<Integer> productIds;
    /**
     * 规格id
     */
    private List<Integer> productSpecIds;
}
