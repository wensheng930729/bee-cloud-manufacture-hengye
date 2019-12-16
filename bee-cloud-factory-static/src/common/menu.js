/* eslint no-useless-escape:0 */
const reg = /(((^https?:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)$/g

export function isUrl(path) {
  return reg.test(path)
}

const menuData = [
  {
    name: '设备管理',
    icon: 'setting',
    path: 'device',
    children: [
      {
        name: '电表管理',
        path: 'ammeter'
      }, {
        name: '电价管理',
        path: 'electricityPrice'
      }, {
        name: '设备巡检',
        path: 'inspection'
      }, {
        name: '设备管理',
        path: 'deviceManage'
      }, {
        name: '称重设备管理',
        path: 'weighDevice'
      }, {
        name: 'PLC设备管理',
        path: 'plcManage'
      }, {
        name: '网关管理',
        path: 'gateway'
      }
    ]
  }, {
    name: '产品管理',
    icon: 'bars',
    path: 'product',
    children: [
      {
        name: '产品档案',
        path: 'archives'
      }, {
        name: '产品类别',
        path: 'productCategory'
      }
    ]
  }, {
    name: '质检管理',
    icon: 'ci',
    path: 'quality',
    children: [
      {
        name: '化验属性',
        path: 'labProp'
      }
    ]
  }, {
    name: '物流管理',
    icon: 'diff',
    path: 'logistical',
    children: [
      {
        name: '地点配置',
        path: 'locationConfig'
      }
    ]
  }, {
    name: '仓库管理',
    icon: 'apartment',
    path: 'warehouse',
    children: [
      {
        name: '仓库档案',
        path: 'warehouseFile'
      }, {
        name: '期初库存',
        path: 'stock'
      }
    ]
  }, {
    name: '统计配置',
    icon: 'reconciliation',
    path: 'statistics',
    children: [
      {
        name: '原料吨耗配置',
        path: 'materialCons'
      }, {
        name: '原料损耗配置',
        path: 'lossConfig'
      }, {
        name: '看板BI配置',
        path: 'boardConfig'
      }, {
        name: '报表配置',
        path: 'reportConfig'
      }
    ]
  }, {
    name: '客户及供应商管理',
    icon: 'usergroup-add',
    path: 'crm',
    children: [
      {
        name: '客户管理',
        path: 'clientManage'
      }, {
        name: '供应商管理',
        path: 'supplierManage'
      }, {
        name: '上下游账户管理',
        path: 'accountManage'
      }
    ]
  }, {
    name: '权限配置',
    icon: 'block',
    path: 'permission',
    children: [
      {
        name: '人员管理',
        path: 'userManage'
      }, {
        name: '角色管理',
        path: 'roleManage'
      }
    ]
  }
]

function formatter(data, parentPath = '/', myData = [], first) {
  let new_menus = [];
  if (first) {
    data.forEach(item => {
      myData.forEach(every => {
        if (item.path === every.path) {
          new_menus.push(item)
        }
      })
    })
  } else {
    new_menus = [].concat(data)
  }

  return new_menus.map(item => {
    let { path } = item;

    if (!isUrl(path)) {
      path = parentPath + item.path
    }

    const result = { ...item, path }

    if (item.children) {
      result.children = formatter(item.children, `${parentPath}${item.path}/`, [], false)
    }
    return result
  })
}

export const getMenuData = (menus) => formatter(menuData, '/', menus, true)
