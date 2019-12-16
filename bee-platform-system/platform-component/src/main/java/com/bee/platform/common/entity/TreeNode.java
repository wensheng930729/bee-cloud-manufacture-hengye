package com.bee.platform.common.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author chengke
 * @version 1.0.0
 * @ClassName TreeNode
 * @Description 地区树
 * @Date 2019/4/28 15:34
 **/

@Data
@AllArgsConstructor
@Accessors(chain = true)
public class TreeNode {

    private static final long serialVersionUID = 1L;
    /**
     * id
     */
    private Integer id;
    /**
     * 父级地区关系
     */
    private Integer pid;
    /**
     * 地区名称
     */
    private String district;

    /**
     * 子属级别关系
     */
    private Integer level;

    private List<TreeNode> children;

    public TreeNode() {
    }

    public TreeNode(Integer id, String district, Integer pid, Integer level) {
        this.id = id;
        this.pid = pid;
        this.district = district;
        this.level = level;
    }

    public TreeNode(Integer id, String district, Integer level, TreeNode parent) {
        this.id = id;
        this.pid = parent.getId();
        this.district = district;
        this.level = level;
    }


}
