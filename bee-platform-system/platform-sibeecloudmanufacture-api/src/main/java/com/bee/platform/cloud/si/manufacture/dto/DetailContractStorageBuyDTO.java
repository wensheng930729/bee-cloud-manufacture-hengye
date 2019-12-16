package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Date;

@Data
@Accessors(chain = true)
@ApiModel(value = "扫描二维码查询-采购-合同仓库相关数据")
public class DetailContractStorageBuyDTO {

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "称重时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm:ss")
    private Date weighingTime;

    @ApiModelProperty(value = "司磅员")
    private String weightMan;

    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;

    @ApiModelProperty(value = "仓库id")
    private Integer storageId;

    @ApiModelProperty(value = "仓库名称")
    private String storageName;

    @ApiModelProperty(value = "入库时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm:ss")
    private LocalDateTime storageTime;
}
