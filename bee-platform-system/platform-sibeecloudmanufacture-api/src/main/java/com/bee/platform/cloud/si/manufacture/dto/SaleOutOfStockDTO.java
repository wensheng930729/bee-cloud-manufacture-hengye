package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: SaleOutOfStockDTO
 * @Description: 销售已出库信息
 * @Author: fei.sun
 * @Date: 2019/9/26 17:17
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售已出库信息")
public class SaleOutOfStockDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("业务唯一标识id")
    private String contractCarId;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("承运商")
    private String carrierName;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("司机名称")
    private String driverName;

    @ApiModelProperty("联系方式")
    private String contact;

    @ApiModelProperty("出库数量")
    private BigDecimal number;

    @ApiModelProperty("出库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDateTime modifyTime;
}
