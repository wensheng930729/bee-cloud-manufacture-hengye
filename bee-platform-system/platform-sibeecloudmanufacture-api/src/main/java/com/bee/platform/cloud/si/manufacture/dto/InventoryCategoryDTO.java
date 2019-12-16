package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-11-26 09:48
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("根据盘点分类获得下拉列表详细")
public class InventoryCategoryDTO implements Serializable {

    private static final long serialVersionUID = 3144219270170591162L;

    @ApiModelProperty("分类后具体类别的业务id")
    private Integer categoryId;

    @ApiModelProperty("分类后具体类别的详细描述")
    private String categoryName;

    public InventoryCategoryDTO(Integer categoryId, String categoryName) {
        this.categoryId = categoryId;
        this.categoryName = categoryName;
    }
}
