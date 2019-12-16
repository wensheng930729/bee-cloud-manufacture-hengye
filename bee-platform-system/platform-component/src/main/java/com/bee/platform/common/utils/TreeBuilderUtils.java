package com.bee.platform.common.utils;

import com.bee.platform.common.entity.TreeNode;

import java.util.ArrayList;
import java.util.List;

/**
 * @author chengke
 * @version 1.0.0
 * @ClassName TreeBuilderUtils
 * @Description 建树形结构
 * @Date 2019/4/28 15:53
 **/
public class TreeBuilderUtils {

    /**
     * 两层循环实现建树
     * @param treeNodes 传入的树节点列表
     * @return
     */
    public static List<TreeNode> bulid(List<TreeNode> treeNodes) {

        List<TreeNode> trees = new ArrayList<TreeNode>();

        for (TreeNode treeNode : treeNodes) {

            if (0==(treeNode.getPid())) {
                trees.add(treeNode);
            }

            for (TreeNode it : treeNodes) {
                if (it.getPid().equals(treeNode.getId())) {
                    if (treeNode.getChildren() == null) {
                        treeNode.setChildren(new ArrayList<TreeNode>());
                    }
                    treeNode.getChildren().add(it);
                }
            }
        }
        return trees;
    }


    /**
     * 使用递归方法建树
     * @param treeNodes
     * @return
     */
    public static List<TreeNode> buildByRecursive(List<TreeNode> treeNodes) {
        List<TreeNode> trees = new ArrayList<TreeNode>();
        for (TreeNode treeNode : treeNodes) {
            if (0==treeNode.getPid()) {
                trees.add(findChildren(treeNode,treeNodes));
            }
        }
        return trees;
    }

    /**
     * 递归查找子节点
     * @param treeNodes
     * @return
     */
    public static TreeNode findChildren(TreeNode treeNode,List<TreeNode> treeNodes) {
        for (TreeNode it : treeNodes) {
            if(treeNode.getId().equals(it.getPid())) {
                if (treeNode.getChildren() == null) {
                    treeNode.setChildren(new ArrayList<TreeNode>());
                }
                treeNode.getChildren().add(findChildren(it,treeNodes));
            }
        }
        return treeNode;
    }


}
