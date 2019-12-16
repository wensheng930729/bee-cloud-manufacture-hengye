package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName: ContractCarDTO
 * @Description: 待装货车辆信息
 * @Author: fei.sun
 * @Date: 2019/9/26 17:17
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("待装货车辆信息")
public class ContractCarDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("业务唯一标识id")
    private String contractCarId;

    @ApiModelProperty("是否已出库（0未出库；1已出库）")
    private Integer outStorage;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("承运商")
    private String carrierName;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("收货重量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物数量单位")
    private String productUnit;

    @ApiModelProperty("司机名称")
    private String driverName;

    @ApiModelProperty("联系方式")
    private String contact;
}
