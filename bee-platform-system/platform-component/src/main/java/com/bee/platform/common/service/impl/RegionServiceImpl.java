package com.bee.platform.common.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumComonRegionLevel;
import com.bee.platform.common.dao.mapper.RegionMapper;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.Region;
import com.bee.platform.common.entity.RegionInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.TreeNode;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.TreeBuilderUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Created by CrazyMouse on 2016/11/28.
 */
@Slf4j
@Service
public class RegionServiceImpl extends ServiceImpl<RegionMapper, Region> implements RegionService {

    @Autowired
    private RegionMapper regionMapper;

    @Autowired
    private JedisService jedisService;

    @Override
    public List<Region> findRegionByParentId(int pid) {
        return regionMapper.selectList(new EntityWrapper<Region>().eq("pid", pid));
    }


    @Override
    public RegionInfo findRegionById(Integer id) {
        try {
            RegionInfo value = jedisService.getHash(ConstantsUtil.ALL_REGION_KEY, id, RegionInfo.class);
            if (value == null) {
                Map<Integer, RegionInfo> map = selectAll();
                jedisService.delKey(ConstantsUtil.ALL_REGION_KEY);
                jedisService.setHash(ConstantsUtil.ALL_REGION_KEY, map);
                return map.get(id);
            }
            return value;
        } catch (Exception e) {
            log.error("缓存连接异常，查询的键是：{}，异常信息是：{}", ConstantsUtil.ALL_REGION_KEY, e);
            return selectAll().get(id);
        }
    }

    /**
     * @notes 用户信息根据RegionId返回RegionDTO对象
     * @Author junyang.li
     * @Date 13:50 2019/3/6
     **/
    @Override
    public RegionDTO selectRegion(String regionId) {
        RegionDTO dto = new RegionDTO();
        if (StringUtils.isEmpty(regionId) || ConstantsUtil.MINUS.equals(regionId)) {
            return dto;
        }
        //获取到区级信息
        RegionInfo county = findRegionById(Integer.valueOf(regionId));
        // 如果根据id查询到的不是区县级，则不进行操作
        if (!EnumComonRegionLevel.level.four.getKey().equals(county.getLevel())) {
            return dto;
        }
        RegionInfo city=null;
        RegionInfo province=null;
        if(!ObjectUtils.isEmpty(county)){
            //获取到市级信息
            city = findRegionById(county.getPid());
        }
        if(!ObjectUtils.isEmpty(city)){
            //获取到省级信息
            province = findRegionById(city.getPid());
        }
        return dto.setCounty(county).setCity(city).setProvince(province);
    }


    private Map<Integer, RegionInfo> selectAll() {
        List<RegionInfo> regions = BeanUtils.assemble(RegionInfo.class, regionMapper.selectList(new EntityWrapper<>()));
        return regions.stream().collect(Collectors.toMap(RegionInfo::getId, regionInfo -> regionInfo));
    }



    @Override
    public List<TreeNode> getAllRegion() {
        try {
            List<TreeNode> treeNodes;
            //先从缓存中获取数据
            treeNodes = jedisService.getJsonArrayObject(ConstantsUtil.ALL_REGION_TREE_NODE_KEY,TreeNode.class);
            if (ObjectUtils.isEmpty(treeNodes)) {
                // 从数据库读取
                treeNodes = getRegionTreeNodes();
                jedisService.delKey(ConstantsUtil.ALL_REGION_TREE_NODE_KEY);
                jedisService.setJsonObject(ConstantsUtil.ALL_REGION_TREE_NODE_KEY, treeNodes, 0);
            }
            return treeNodes;
        }catch (Exception e){
            log.error("缓存连接异常，查询的键是：{}，异常信息是：{}", ConstantsUtil.ALL_REGION_TREE_NODE_KEY, e);
            return getRegionTreeNodes();
        }
    }

    private List<TreeNode> getRegionTreeNodes() {
        List<TreeNode> treeNodes;
        log.info("从数据库查询所有地区信息");
        List<Region> regions = regionMapper.selectList(new EntityWrapper<Region>());
        List<TreeNode> all = BeanUtils.assemble(TreeNode.class, regions);
        List<TreeNode> country = all.stream().filter(o -> o.getLevel().equals(0)).collect(Collectors.toList());
        List<TreeNode> list = Lists.newArrayList(country);
        country.forEach(g -> {
            List<TreeNode> province = all.stream().filter(o -> o.getPid().equals(g.getId())).collect(Collectors.toList());
            list.addAll(province);
            province.forEach(p -> {
                List<TreeNode> city = all.stream().filter(o -> o.getPid().equals(p.getId())).collect(Collectors.toList());
                list.addAll(city);
                city.forEach(c -> {
                    List<TreeNode> county = all.stream().filter(o -> o.getPid().equals(c.getId())).collect(Collectors.toList());
                    list.addAll(county);
                });
            });
        });
        treeNodes = TreeBuilderUtils.buildByRecursive(list);
        return treeNodes;
    }

    @Override
    public List<Region> getAllProvince() {
        return regionMapper.selectList(new EntityWrapper<>(new Region().setLevel(EnumComonRegionLevel.level.one.getKey())));
    }

    @Override
    public String assembleFullAddress(Integer districtId, String street) {
        if (districtId == null){
            return null;
        }
        ResponseResult<Map<String, Object>> ret = this.findAllRegionById(districtId);
        Map<String, Object> returnMap = (Map<String, Object>) ret.getObject();
        if (returnMap == null) {
            log.info("获取地址信息失败, districtId: {}", districtId);
            return null;
        }
        Region province = (Region) returnMap.get("province");
        Region city = (Region) returnMap.get("city");
        Region county = (Region) returnMap.get("county");
        StringBuilder sb = new StringBuilder();
        sb.append(province.getDistrict());
        sb.append(" " + city.getDistrict());
        sb.append(" " + county.getDistrict());
        sb.append(" " + (StringUtils.isEmpty(street) ? "" :street));
        return sb.toString();
    }


    @Override
    public ResponseResult<Map<String, Object>> findAllRegionById(Integer id) {
        try {
            //获取到区级信息
            Region county = selectById(id);
            // 如果根据id查询到的不是区县级，则不进行操作
            if (ObjectUtils.isEmpty(county) || !EnumComonRegionLevel.level.four.getKey().equals(county.getLevel())) {
                return ResponseResult.fail("查询失败");
            }
            Region city = null;
            Region province = null;
            if (!ObjectUtils.isEmpty(county)) {
                //获取到市级信息
                city = selectById(county.getPid());
            }
            if (!ObjectUtils.isEmpty(city)) {
                //获取到省级信息
                province = selectById(city.getPid());
            }
            Map<String, Object> returnMap = new HashMap<String, Object>();
            returnMap.put("province", province);
            returnMap.put("city", city);
            returnMap.put("county", county);
            return ResponseResult.success(returnMap);
        } catch (Exception e) {
            log.error("查询地址异常", e);
        }
        return ResponseResult.fail("查询失败");
    }
}
