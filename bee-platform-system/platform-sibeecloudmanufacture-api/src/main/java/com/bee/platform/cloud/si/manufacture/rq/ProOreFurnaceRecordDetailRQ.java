package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "矿热炉记录明细请求参数")
public class ProOreFurnaceRecordDetailRQ implements Serializable {
    private static final long serialVersionUID = 9136769761617329748L;

    @ApiModelProperty("矿热炉记录主表id")
    private Long oreRecordId;

    @ApiModelProperty("捣炉记录：json数据")
    private List<ProStirFurnaceRecordRQ> stirFurnaceRecordList;

    @ApiModelProperty("电极消耗记录请求参数")
    private List<ProElectrodeRQ> electrodeList;

    @ApiModelProperty("出炉记录请求参数")
    private ProOutFurnaceRecordRQ outFurnaceRecordRQ;

    @ApiModelProperty("配电记录请求参数")
    private ProPowerRecordRQ powerRecordRQ;
}
