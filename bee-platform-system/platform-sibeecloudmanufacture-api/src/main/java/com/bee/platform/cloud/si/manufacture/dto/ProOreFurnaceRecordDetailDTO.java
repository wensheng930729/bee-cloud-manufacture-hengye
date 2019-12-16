package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "矿热炉记录明细返回信息")
public class ProOreFurnaceRecordDetailDTO implements Serializable {

    private static final long serialVersionUID = -6461244518547421963L;

    @ApiModelProperty("矿热炉记录主表id")
    private Long oreRecordId;

    @ApiModelProperty("捣炉记录返回信息")
    private List<ProStirFurnaceRecordDTO> stirFurnaceRecordList;

    @ApiModelProperty("电极消耗记录返回信息")
    private List<ProElectrodeDTO> electrodeList;

    @ApiModelProperty("出炉记录返回信息")
    private ProOutFurnaceRecordDTO outFurnaceRecordDTO;

    @ApiModelProperty("配电记录返回信息")
    private ProPowerRecordDTO powerRecordDTO;
}
