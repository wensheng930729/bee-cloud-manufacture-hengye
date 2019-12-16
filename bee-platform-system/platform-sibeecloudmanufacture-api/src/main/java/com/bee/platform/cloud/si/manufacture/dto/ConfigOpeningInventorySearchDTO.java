package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ConfigOpeningInventorySearchDTO
 * @Description 功能描述
 * @Date 2019/6/19 10:57
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存条件搜索列表返回信息")
public class ConfigOpeningInventorySearchDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;


    @ApiModelProperty("期初库存编号")
    private String code;

    @ApiModelProperty("期初日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openingInventoryTime;

    @ApiModelProperty("备注")
    private String remark;




}
