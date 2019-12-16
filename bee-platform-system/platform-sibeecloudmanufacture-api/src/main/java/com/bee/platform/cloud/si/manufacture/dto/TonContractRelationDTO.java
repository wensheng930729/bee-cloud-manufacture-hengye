package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName: TonContractRelationDTO
 * @Description: 吨袋code和合同业务id关联DTO
 * @Author: liliang
 * @Date: 2019/9/29 10:24
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("吨袋code和合同业务id关联DTO")
public class TonContractRelationDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("吨袋编号")
    private String tonCode;

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

}
