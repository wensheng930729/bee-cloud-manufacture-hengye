export default [
  {
    path: '/login',
    component: './login',
  },
  {
    path: '/register',
    component: './register',
  },
  {
    path: '/forgetPassword',
    component: './forgetPassword',
  },
  // app
  {
    path: '/',
    component: '../layouts/index',
    Routes: ['src/pages/Authorized'],
    routes: [
      {
        path: '/',
        redirect: '/board',
      },
      {
        path: '/board',
        name: '总览',
        component: './board',
      },
      {
        path: '/reportForm',
        name: '报表',
        // redirect: '/reportForm/inventory',
        routes: [
          {
            path: '/reportForm/inventory',
            name: '库存报表',
            component: './reportForm/inventory',
          },
          {
            path: '/reportForm/qualityInspection',
            name: '质检报表',
            component: './reportForm/qualityInspection',
          },
          {
            path: '/reportForm/purchase',
            name: '采购报表',
            component: './reportForm/purchase',
          },
          {
            path: '/reportForm/sales',
            name: '销售报表',
            component: './reportForm/sales',
          },
          {
            path: '/reportForm/yieldAnalysis',
            name: '产量分析',
            component: './reportForm/yieldAnalysis',
          },
          {
            path: '/reportForm/passRate',
            name: '合格率',
            component: './reportForm/passRate',
          },
          {
            path: '/reportForm/productConsumptAnaly',
            name: '产量、消耗分析表',
            component: './reportForm/productConsumptAnaly',
          },
          {
            path: '/reportForm/logistics',
            name: '物流报表',
            component: './reportForm/logistics',
          },
        ],
      },
      {
        path: '/purchase',
        name: '采购管理',
        routes: [
          {
            path: '/purchase',
            redirect: '/purchase/order/index',
          },
          {
            path: '/purchase/order',
            name: '采购订单',
            isHide: true,
            routes: [
              {
                path: '/purchase/order',
                redirect: '/purchase/order/index',
              },
              {
                path: '/purchase/order/index',
                name: '订单管理',
                component: './purchase/index',
              },
              {
                path: '/purchase/order/detail',
                name: '订单详情',
                component: './purchase/detail/index',
              },
              {
                path: '/purchase/order/add',
                name: '新增合同',
                component: './purchase/add/index',
              },
            ],
          }, {
            path: '/purchase/settle',
            name: '采购结算',
            component: './purchase/settle/index',
          }]
      },
      {
        path: '/sale',
        name: '销售订单',
        isHide: true,
        routes: [
          {
            path: '/sale',
            name: '订单管理',
            component: './sale/index',
          },
          {
            path: '/sale/detail',
            name: '订单详情',
            component: './sale/detail/index',
          },
          {
            path: '/sale/add',
            name: '新增合同',
            component: './sale/add/index',
          },
        ],
      },
      {
        path: '/inventoryManage',
        name: '库存管理',
        routes: [
          {
            path: '/inventoryManage/purchaseWarehousing',
            name: '采购入库',
            routes: [
              {
                path: '/inventoryManage/purchaseWarehousing',
                redirect: '/inventoryManage/purchaseWarehousing/index',
              },
              {
                path: '/inventoryManage/purchaseWarehousing/index',
                component: './InventoryManage/purchaseWarehousing',
              },
              {
                isHide: true,
                path: '/inventoryManage/purchaseWarehousing/detail',
                component: './InventoryManage/purchaseWarehousing/Detail',
              },
            ],
          },

          {
            path: '/inventoryManage/productStorage',
            name: '成品入库',
            component: './InventoryManage/productStorage',
          },

          {
            path: '/inventoryManage/weightMachine',
            name: '地磅单',
            component: './InventoryManage/weightMachine',
          },

          {
            path: '/inventoryManage/salesOutStock',
            name: '销售出库',
            routes: [
              {
                path: '/inventoryManage/salesOutStock',
                redirect: '/inventoryManage/salesOutStock/index',
              },
              {
                path: '/inventoryManage/salesOutStock/index',
                component: './InventoryManage/salesOutStock',
              },
              {
                isHide: true,
                path: '/inventoryManage/salesOutStock/detail',
                component: './InventoryManage/salesOutStock/Detail',
              },
            ],
          },

          {
            path: '/inventoryManage/existingQuantityQuery',
            name: '现存量查询',
            component: './InventoryManage/existingQuantityQuery',
          },

          {
            path: '/inventoryManage/inventory',
            name: '盘点',
            isHide: true,
            routes: [
              {
                path: '/inventoryManage/inventory',
                redirect: '/inventoryManage/inventory/index',
              },
              {
                path: '/inventoryManage/inventory/index',
                component: './InventoryManage/inventory',
                name: '盘点列表',
              },
              {
                path: '/inventoryManage/inventory/Edit',
                component: './InventoryManage/inventory/Edit',
                name: '新增',
              },
              {
                path: '/inventoryManage/inventory/detail',
                component: './InventoryManage/inventory/Detail',
                name: '盘点详情',
              },
            ],
          },
        ],
      },
      {
        path: '/weighbridge',
        name: '磅房管理',
        routes: [
          {
            path: '/weighbridge',
            redirect: '/weighbridge/purchase/index',
          }, {
            isHide: true,
            path: '/weighbridge/purchase',
            name: '采购过磅',
            routes: [
              {
                path: '/weighbridge/purchase',
                redirect: '/weighbridge/purchase/index',
              },
              {
                path: '/weighbridge/purchase/index',
                name: '过磅车辆列表',
                component: './weighbridge/purchase/index',
              },
              {
                path: '/weighbridge/purchase/add',
                name: '新增称重',
                component: './weighbridge/purchase/add',
              },
              {
                path: '/weighbridge/purchase/edit',
                name: '开始称重',
                component: './weighbridge/purchase/edit',
              },
              {
                path: '/weighbridge/purchase/detail',
                name: '磅单详情',
                component: './weighbridge/purchase/detail',
              },
            ],
          },
          {
            isHide: true,
            path: '/weighbridge/sale',
            name: '销售过磅',
            routes: [
              {
                path: '/weighbridge/sale',
                redirect: '/weighbridge/sale/index',
              },
              {
                path: '/weighbridge/sale/index',
                name: '过磅车辆列表',
                component: './weighbridge/sale/index',
              },
              {
                path: '/weighbridge/sale/add',
                name: '新增称重',
                component: './weighbridge/sale/add',
              },
              {
                path: '/weighbridge/sale/edit',
                name: '开始称重',
                component: './weighbridge/sale/edit',
              },
              {
                path: '/weighbridge/sale/detail',
                name: '磅单详情',
                component: './weighbridge/sale/detail',
              },
            ],
          },
        ],
      },
      {
        path: '/logisticsManage',
        name: '物流管理',
        routes: [
          {
            path: '/logisticsManage/contractStatement',
            name: '合同外磅单',
            component: './contractStatement'
          },
          {
            path: '/logisticsManage/order',
            name: '物流订单',
            isHide: true,
            routes: [
              {
                path: '/logisticsManage/order',
                redirect: '/logisticsManage/order/index',
              },
              {
                path: '/logisticsManage/order/index',
                component: './logisticsOrder',
              },
              {
                path: '/logisticsManage/order/detail',
                component: './logisticsOrder/logisticsOrderDetail',
              },
              {
                path: '/logisticsManage/order/transportDetail',
                component: './logisticsOrder/transportDetail',
              },
            ],
          },
        ],
      }
    ],
  },
];
