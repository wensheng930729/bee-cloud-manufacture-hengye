package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * @ClassName: SaleOutOfStockDetailDTO
 * @Description: 销售已出库详情
 * @Author: fei.sun
 * @Date: 2019/9/26 17:17
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售已出库详情")
public class SaleOutOfStockDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("业务唯一标识id")
    private String contractCarId;

    @ApiModelProperty("是否已出库（0未出库；1已出库）")
    private Integer outStorage;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("出库时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDateTime modifyTime;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("司机名称")
    private String driverName;

    @ApiModelProperty("联系方式")
    private String contact;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("总出库数量")
    private BigDecimal number;

    @ApiModelProperty("出库信息")
    List<FinishedProductFreeOutDetailDTO> oneList;

    @ApiModelProperty("吨袋出库信息")
    List<SaleOutStorageTonBagDTO> twoList;



}
